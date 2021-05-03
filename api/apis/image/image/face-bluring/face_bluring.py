from typing import Optional
from fastapi import APIRouter
from fastapi import File, UploadFile
import io
from starlette.responses import StreamingResponse
import cv2
import face_recognition
router = APIRouter()
import numpy as np
from skimage.filters import gaussian


@router.post("/latest/", summary="Face bluring SOTA")
@router.post("/v1/")
async def face_bluring_v1(file: UploadFile = File(...), sigma: Optional[int] = 50):
    contents = await file.read()
    nparr = np.fromstring(contents, np.uint8)
    image = cv2.imdecode(nparr, cv2.IMREAD_COLOR)

    locations = face_recognition.face_locations(image)
 
    for location in locations:
        startY = location[0]
        endY = location[2]
        startX = location[1]
        endX = location[3]
        image = blur(image, startX, endX, startY, endY, sigma=sigma)
    
    res, im_png = cv2.imencode(".png", image)
    
    return StreamingResponse(io.BytesIO(im_png.tobytes()), media_type="image/png")
    
    
    
def blur(image, x0, x1, y0, y1, sigma=1, multichannel=True):
    y0, y1 = min(y0, y1), max(y0, y1)
    x0, x1 = min(x0, x1), max(x0, x1)
    im = image.copy()
    sub_im = im[y0:y1,x0:x1].copy()
    blur_sub_im = gaussian(sub_im, sigma=sigma, multichannel=multichannel)
    blur_sub_im = np.round(255 * blur_sub_im)
    im[y0:y1,x0:x1] = blur_sub_im
    return im
