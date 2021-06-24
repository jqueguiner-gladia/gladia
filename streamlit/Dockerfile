FROM python:3.7

RUN apt-get update && apt-get install -y \
    ffmpeg \
    libsm6 \
    libxext6

EXPOSE 8501

WORKDIR /app

COPY requirements.txt ./requirements.txt

RUN pip3 install -r requirements.txt

CMD streamlit run app.py
