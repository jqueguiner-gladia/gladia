import tempfile
import numpy as np

from PIL import Image


def predict(image):
    from ISR.models import RDN, RRDN

    # open image from path
    img = Image.open(image)

    model_gans = RRDN(weights="gans")
    output = tempfile.NamedTemporaryFile(suffix=".png", delete=False).name

    img = model_gans.predict(np.array(img))
    Image.fromarray(img).save(output)

    return output
