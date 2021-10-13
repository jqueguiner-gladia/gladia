import re
from numpy.core.numeric import outer
from requests.models import Response
import streamlit as st
import requests
from PIL import Image
import io
import asyncio
import aiohttp
import aiofiles
from icecream import ic
import time
from streamlit.uploaded_file_manager import UploadedFileRec, UploadedFile
import json
from unifai-api-utils.io import get_buffer_type, get_buffer_category
import pandas as pd
import time
import urllib

from cleantext import clean


environments = {
	'dev': {
		'url': 'http://146.59.193.116',
		'port': 5000,
	},
	'prod': {
		'url': 'https://paperswithapi.com',
		'port': 443 
	},

}

modes = {
	'unit',
	'sota'
}

env = st.selectbox('environment', list(environments))

mode = st.selectbox('mode', list(modes))


endpoint = f"{environments[env]['url']}:{environments[env]['port']}"

r = requests.get(f"{endpoint}/openapi.json")

schema = r.json()
SOTA = list()

for path in schema["paths"]:
	SOTA.append(path)


task = st.selectbox(
	'Which task do you want to compare ?',
	SOTA
	)


collapse = st.checkbox(label="collapse results (if applicable)", value=False)

models = list()
parameters = list()

for parameter in schema["paths"][task]["post"]["parameters"]:
	if parameter["schema"]["title"] == "Model":
		models = parameter["schema"]["enum"]
	else:
		parameters.append(parameter)

if mode == "unit":
	models = st.selectbox(
		'Which model do you want to run ?',
		models
		)
	models = [models]
	


operation_id = schema["paths"][task]["post"]["operationId"]


request_bodies = list()
if f"Body_{operation_id}" in schema["components"]["schemas"]:
	request_bodies.append(schema["components"]["schemas"][f"Body_{operation_id}"])


form_data = dict()
with st.form(key='my_form'):
	for parameter in parameters:
		
		if parameter["schema"]["type"] == "string":
			form_data[parameter["name"]] = st.text_input(label=parameter["schema"]["title"], value=parameter["schema"]["default"])

		if parameter["schema"]["type"] == "integer":
			form_data[parameter["name"]] = st.slider(parameter["schema"]["title"], 0, parameter["schema"]["default"]*2, parameter["schema"]["default"])

	for request_body in request_bodies:
		for name, property in request_body["properties"].items():
			form_data[name] = st.file_uploader(label=property["title"])
	
	
	submit_button = st.form_submit_button(label='Submit')


async def predict(url, model, session, headers, params, form_data):

	params["model"] = model
	
	url =  url + "?" + urllib.parse.urlencode(params)

	form = aiohttp.FormData()
	
	for k, v in form_data.items():
		if isinstance(v, UploadedFile):
			v.seek(0)
			form.add_field(k, v.read(), filename=v.name, content_type='application/octet-stream')
		else:
			if k not in params:
				form.add_field(k, v)

	try:
		async with session.post(url=url, data=form, headers=headers) as resp:
			if resp.status == 200:
				output = await resp.read()
				
				return output
			else:
				print(resp.status)

	except Exception as e:
		print("Unable to get url {} due to {}.".format(url, e.__class__))



async def main(models, task, params, form_data):
	
	url = f'{endpoint}{task}'

	headers = {
    	'accept': 'application/json',
	}

	with st.spinner('Comparing SOTA please wait ...'):
		async with aiohttp.ClientSession() as session:
			this_form = form_data
			
			tasks = []
			for model in models:
				tasks.append(asyncio.ensure_future(predict(url, model, session, headers, params, this_form)))

			ret = await asyncio.gather(*tasks)

			
		print("Finalized all. Return is a list of len {} outputs.".format(len(ret)))
		columns = st.beta_columns(len(models))
		for column_idx, model in enumerate(models, start=0):
			columns[column_idx].header(model)
			if ret[column_idx]:
				if get_buffer_category(ret[column_idx]) == "image":
					original = Image.open(io.BytesIO(ret[column_idx]))
					columns[column_idx].image(original, use_column_width=True)
				elif get_buffer_category(ret[column_idx]) == "text":
					try:
						columns[column_idx].write(json.loads(ret[column_idx].decode()))
					except:
						columns[column_idx].write(ret[column_idx])
				else:
					try:
						if collapse:
							columns[column_idx].write(eval(ret[column_idx].decode()))
						else:
							columns[column_idx].write(pd.DataFrame(eval(ret[column_idx])))
					except:
						print(ret[column_idx].decode())
						columns[column_idx].write(ret[column_idx].decode())

			else:
				columns[column_idx].write("")
		


if submit_button:

	params = dict()
	for parameter in parameters:
		if parameter["name"] in form_data:
			params[parameter['name']]=form_data[parameter['name']]
		else:
			if parameter["required"]:
				st.error(f"Error missing {parameter['name']}")

	asyncio.run(main(models, task, params, form_data))
