import streamlit as st
import os
from PIL import Image
import requests
import pandas as pd

from io import StringIO
import io


keys = {
	'asciify': {
		'key': '02371676-01bb-4636-9e1e-c63bd3230d8a', 
		'endpoint': 'process', 
		'input': {
			'file' : {
				'type': 'image'
			}
		},
		'output': {
			'type': 'text'
		}
	},
	'background-removal': {
		'key': 'd9779111-8009-4f76-bb91-20f8374d2085', 
		'endpoint': 'process', 
		'input': {
			'file': {
				'type': 'image'
			}
		},
		'output': {
			'type': 'image/png'
		}
	},
	'car-classification': {
		'key': 'dff3d9ee-9d17-4ba8-a94e-a52ea60cd618', 
		'endpoint': 'detect', 
		'input': {
			'file': {
				'type': 'image'
			}
		},
		'output': {
			'type': 'json'
		}
	},
	'color-transfer': {
		'key': 'e48629ac-001a-4eea-9aea-18acc7ce0d4c', 
		'endpoint': 'process', 
		'input': {
			'source': {
				'type': 'image'
			},
			'target': {
				'type': 'image'
			}
		},
		'output': {
			'type': 'image/png'
		}
	},
	'colorgram': {
		'key': '630f29a1-a3b8-4e55-83bf-ce4a00bc6cfd', 
		'endpoint': 'detect',
		'input': {
			'file': {
				'type': 'image'
			},
			'nb_colors': {
				'type': 'integer',
				'min': 1,
				'max': 20,
				'default': 5,
				'step': 1
			}
		},
		'output': {
			'type': 'json'
		}
	},
	'colorization': {
		'key': 'b4944fab-c885-4fbf-91b4-16c5d0dbd4b1', 
		'endpoint': 'process',
		'input': {
			'file': {
				'type': 'image'
			},
			'render_factor': {
				'type': 'integer',
				'min': 25,
				'max': 65,
				'default': 35,
				'step': 1
			}
		},
		'output': {
			'type': 'image/png'
		}
	},
	'face-blurring': {
		'key': '7325deb7-8c2f-44e9-8e34-13c7562a6125', 
		'endpoint': 'process', 
		'input': {
			'file': {
				'type': 'image'
			}
		},
		'output': {
			'type': 'image/png'
		}
	},
	'deblurring': {
		'key': '77e6ad8d-2cd9-4581-95c0-417590a965fa', 
		'endpoint': 'process', 
		'input': {
			'file': {
				'type': 'image'
			}
		},
		'output': {
			'type': 'image/png'
		}
	},
	'face-detection': {
		'key': '92328f51-4cee-45d0-aa69-9aa1ca2f7594', 
		'endpoint': 'detect', 
		'input': {
			'file': {
				'type': 'image'
			}
		},
		'output': {
			'type': 'json'
		}
	},
	'neural-style': {
		'key': 'afc69f2b-d1ad-4c3a-8c25-cbc1ca467cee', 
		'endpoint': 'process', 
		'input': {
			'file': {
				'type': 'image'
			},
			'style': {
				'type': 'select',
				'options': {
					'beret-picasso',
					'kandinsky',
					'seated-nude',
					'shipwreck',
					'starry-night',
					'the_scream',
					'van-gogh',
					'woman-with-hat-matisse'
				}
			}
		},
		'output': {
			'type': 'image/png'
		}
	},
	'nudity-detection': {
		'key': '7ab3ed05-a229-4f3a-b9f9-e58fb77d2137', 
		'endpoint': 'detect', 
		'input': {
			'file': {
				'type': 'image'
			},
		},
		'output': {
			'type': 'json'
		}
	},
	'ocr': {
		'key': '3132302d-fd8b-42ec-9fa6-46526f5291bf', 
		'endpoint': 'detect', 
		'input': {
			'file': {
				'type': 'image'
			}
		},
		'output': {
			'type': 'text'
		}
	},
	'recognition': {
		'key': 'e7c1086f-dce7-424e-9676-bb4c20a14d0c', 
		'endpoint': 'detect', 
		'input': {
			'file': {
				'type': 'image'
			},
			'top_k': {
				'type': 'integer',
				'min': 1,
				'max': 100,
				'default': 5,
				'step': 1
			}
		},
		'output': {
			'type': 'json'
		}
	},
	'rotations-and-flips': {
		'key': 'eeae11a1-cea3-4400-b1ba-ae2b7033e2a9', 
		'endpoint': 'rotate', 
		'input': {
			'file': {
				'type': 'image'
			},
			'angle': {
				'type': 'integer',
				'min': 1,
				'max': 359,
				'default': 30,
				'step': 1
			},
			'cropping': {
				'type': 'select',
				'options': {
					'true',
					'false'
				}
			}
		},
		'output': {
			'type': 'image/png'
		}
	},
	# 'image-rotation-and-flip': {
	# 	'key': 'eeae11a1-cea3-4400-b1ba-ae2b7033e2a9', 
	# 	'endpoint': 'flip', 
	# 	'input': {
	# 		'file': {
	# 			'type': 'image'
	# 		},
	# 		'mode': {
	# 			'type': 'select',
	# 			'options': {
	# 				'true',
	# 				'false'
	# 			}
	# 		}
	# 	},
	# 	'output': {
	# 		'type': 'image/png'
	# 	}
	# },
	# 'image-rotation-and-flip': {
	# 	'key': 'eeae11a1-cea3-4400-b1ba-ae2b7033e2a9', 
	# 	'endpoint': 'crop', 
	# 	'input': {
	# 		'file': {
	# 			'type': 'image'
	# 		},
	# 		'x1': {
	# 			'type': 'integer',
	# 			'min': 1,
	# 			'max': 999999,
	# 			'default': 1,
	# 			'step': 1
	# 		},
	# 		'y1': {
	# 			'type': 'integer',
	# 			'min': 1,
	# 			'max': 999999,
	# 			'default': 1,
	# 			'step': 1
	# 		},
	# 		'x2': {
	# 			'type': 'integer',
	# 			'min': 1,
	# 			'max': 999999,
	# 			'default': 1,
	# 			'step': 1
	# 		},
	# 		'y2': {
	# 			'type': 'integer',
	# 			'min': 1,
	# 			'max': 999999,
	# 			'default': 1,
	# 			'step': 1
	# 		},
	# 	},
	# 	'output': {
	# 		'type': 'image/png'
	# 	}
	#},
	'scene-recognition': {
		'key': '75ea1dab-7113-4db0-a172-74125e176acf', 
		'endpoint': 'detect', 
		'input': {
			'file': {
				'type': 'image'
				},
			'model': {
				'type': 'options'
				}
			},
		'output': {
			'type': 'json'
		}
	},
	'segmentation': {
		'key': '4993fab1-3d8f-41c7-af29-6fd66776757d', 
		'endpoint': 'process', 
		'input': {
			'file': {
				'type': 'image'
				},
			'model': {
				'type': 'select',
				'options': {
					'scene_parsing',
					'cityscapes'
					}
				},
			},
		'output': {
			'type': 'image/png'
		}
	},
	'selfie2anime': {
		'key': '698072b8-9516-45bb-b608-3e159202cba8', 
		'endpoint': 'process', 
		'input': {
			'file': {
				'type': 'image'
				}
			},
		'output': {
			'type': 'image/png'
		}
	},
	'similarity': {
		'key': 'a562c236-8f71-4263-b060-0aca4f8c4abc', 
		'endpoint': 'detect', 
		'input': {
			'file_a': {
				'type': 'image'
				},
			'file_b': {
				'type': 'image'
				}
		},
		'output': {
			'type': 'json'
		}
	},
	'smart-thumbnail': {
		'key': '9ab96d9a-98a6-4d59-846f-f4f95c4bbe7b', 
		'endpoint': 'process', 
		'input': {
			'file': {
				'type': 'image'
			},
			'height': {
				'type': 'integer',
				'min': 1,
				'max': 359,
				'default': 30,
				'step': 1
			},
			'width': {
				'type': 'integer',
				'min': 1,
				'max': 359,
				'default': 30,
				'step': 1
			}
		},
		'output': {
			'type': 'image/png'
		}
	},
	'super-resolution': {
		'key': '44f95e3f-fe5e-4826-90cf-528927954332',
		'endpoint': 'process', 
		'input': {
			'file': {
				'type': 'image'
			}
		},
		'output': {
			'type': 'image/png'
		}
	},
	'text-blurring': {
		'key': '23aa700a-d576-4c83-bf19-9af66d3a18ba', 
		'endpoint': 'process', 
		'input': {
			'file': {
				'type': 'image'
			}
		},
		'output': {
			'type': 'image/png'
		}
	},
	'uncolorization': {
		'key': '9aefbeb2-98a1-4645-b4a6-00ea9593b2ad', 
		'endpoint': 'process', 
		'input': {
			'file': {
				'type': 'image'
			}
		},
		'output': {
			'type': 'image/png'
		}
	}

}



environments = {
	'prod': {
		'url': 'https://api-market-place.ai.ovh.net',
		'port': 443 
	},
	'local': {
		'url': 'http://localhost',
		'port': 5000,
	},
}



#env = st.selectbox('environment', list(environments))
env = "prod"

endpoint = environments[env]['url'] + ':' + str(environments[env]['port'])



apis = list(keys)

api = st.selectbox(
	'Select the API to test',
	apis
	)

multipart = False

data = dict()
for input in keys[api]['input']:

	if keys[api]['input'][input]['type'] == 'image':
		multipart = True

		uploaded_file = st.file_uploader("Choose an Image %s" % input , type=["jpg", "jpeg", "jpg"], key=input)

		if uploaded_file is not None:

			image = Image.open(uploaded_file)

			data[input] = ('input.jpg', uploaded_file.getvalue())


			st.image(image, caption='Input image %s' % input,
				use_column_width=True)

	elif keys[api]['input'][input]['type'] == 'integer':
		number = st.number_input(input, 
			min_value = keys[api]['input'][input]['min'],
			max_value = keys[api]['input'][input]['max'],
			value = keys[api]['input'][input]['default'],
			step = keys[api]['input'][input]['step'])
		
		data[input] = (None, number)
		
	elif keys[api]['input'][input]['type'] == 'select':
		option = st.selectbox(input, list(keys[api]['input'][input]['options']))

		data[input] = (None, option)






if st.button('Process Image'):

	headers = {
		'accept': keys[api]['output']['type'],
		'X-OVH-Api-Key': keys[api]['key'],
	}


	if multipart:
		if env == 'local':
			r = requests.post('%s/%s' % (endpoint, keys[api]['endpoint']) , headers=headers, files=data)
		else:
			r = requests.post('https://api-market-place.ai.ovh.net/image-%s/%s' % (api, keys[api]['endpoint']), headers=headers, files=data)
		st.write(r)
	else:
		if env == 'local':
			r = requests.post(endpoint, headers=headers, data=data)
		else:
			r = requests.post('https://api-market-place.ai.ovh.net/image-%s/%s' % (api, keys[api]['endpoint']), headers=headers, data=data)

	print(r)
	if 'image' in keys[api]['output']['type']:
		result = Image.open(io.BytesIO(r.content))
		
		st.image(result, caption='Output image',
			use_column_width=True)

	elif 'text' in keys[api]['output']['type'] :
		result = r.content
		st.write(result)

	elif 'json' in keys[api]['output']['type'] :
		st.code(r.content, language="json")
		st.table(pd.read_json(r.content))



codes = {
	'php' : '''
<?php
include('vendor/rmccue/requests/library/Requests.php');
Requests::register_autoloader();

$headers = array(
	'accept' => 'image/png',
	'X-OVH-Api-Key' => 'XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX',
	'Content-Type' => 'application/json'
);

$data = '{"url":"https://i.ibb.co/W0JpjrY/input.jpg"}';

$response = Requests::post('https://api-market-place.ai.ovh.net/image-%s/process', $headers, $data);
	''',
	'python' : '''
import requests

headers = {
	'accept': 'image/png',
	'X-OVH-Api-Key': 'XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX',
	'Content-Type': 'application/json',
}

data = '{"url":"https://i.ibb.co/W0JpjrY/input.jpg"}'

response = requests.post('https://api-market-place.ai.ovh.net/image-%s/process', headers=headers, data=data)
	''',
	'NodeJS' : '''
var request = require('request');

var headers = {
	'accept': 'image/png',
	'X-OVH-Api-Key': 'XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX',
	'Content-Type': 'application/json'
};

var dataString = '{"url":"https://i.ibb.co/W0JpjrY/input.jpg"}';

var options = {
	url: 'https://api-market-place.ai.ovh.net/image-%s/process',
	method: 'POST',
	headers: headers,
	body: dataString
};

function callback(error, response, body) {
	if (!error && response.statusCode == 200) {
		console.log(body);
	}
}

request(options, callback);

		''',
	'R' : '''
require(httr)

headers = c(
	`accept` = 'image/png',
	`X-OVH-Api-Key` = 'XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX',
	`Content-Type` = 'application/json'
)

data = '{"url":"https://i.ibb.co/W0JpjrY/input.jpg"}'

res <- httr::POST(url = 'https://api-market-place.ai.ovh.net/image-%s/process', httr::add_headers(.headers=headers), body = data)

		''',
	'Rust' : '''
extern crate reqwest;
use reqwest::headers::*;

fn main() -> Result<(), reqwest::Error> {
	let mut headers = HeaderMap::new();
	headers.insert(ACCEPT, "image/png".parse().unwrap());
	headers.insert(X_OVH_API_KEY, "XXXXXXXX-XXXX-XXXX-XXXX-XXXXXXXXXXXX".parse().unwrap());
	headers.insert(CONTENT_TYPE, "application/json".parse().unwrap());

	let res = reqwest::Client::new()
		.post("https://api-market-place.ai.ovh.net/image-%s/process")
		.headers(headers)
		.body("{"url":"https://i.ibb.co/W0JpjrY/input.jpg"}")
		.send()?
		.text()?;
	println!("{}", res);

	Ok(())
}

		'''
}


#st.header("Code Snippets to use the API")
#language = st.selectbox("Language", (list(codes)))
#st.subheader(language)
#st.code(codes[language] % api, language = language)




