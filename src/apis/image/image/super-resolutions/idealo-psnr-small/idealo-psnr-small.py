def predict(image):
    import os
    from pathlib import Path
    import numpy as np
    from PIL import Image
    from ISR.models import RDN, RRDN
    from PIL import Image
    import tempfile

    #open image from path
    img = Image.open(image)
    lr_img = np.array(img)

    model_gans = RRDN(weights="gans")
    model_noise_cancel = RDN(weights="noise-cancel")
    img = model_gans.predict(np.array(img))
    
    output = tempfile.NamedTemporaryFile(suffix=".png", delete=False).name
    Image.fromarray(img).save(output)
    return output
