import os
import cv2 as cv
import numpy as np

from PIL import Image
from keras.models import load_model
from gladia_api_utils.io import _open
from gladia_api_utils.model_management import download_models


models_to_download = {
    "mlhubber_colorize": {
        "url": "https://huggingface.co/Gladiaio/mlhubber_colorize",
        "output_path": "models",
    }
}

models_path = download_models(models_to_download)

MODEL_FILE = os.path.join(
    models_path["mlhubber_colorize"]["output_path"], "model.06-2.5489.hdf5"
)

PTS_IN_HULL_FILE = os.path.join(
    models_path["mlhubber_colorize"]["output_path"], "pts_in_hull.npy"
)

def predict(image: bytes) -> Image:
    """
    Call the model to return the image colorized

    Args:
        image (bytes): Image to colorize

    Returns:
        Image: Colorized image
    """

    image = _open(image).convert("L")
    image = np.array(image)

    model = load_model(MODEL_FILE)

    h_in, w_in = 256, 256
    h_out, w_out = h_in // 4, w_in // 4
    epsilon = 1e-6
    T = 0.38

    img_rows, img_cols = image.shape[:2]

    q_ab = np.load(PTS_IN_HULL_FILE)
    nb_q = q_ab.shape[0]

    L = image
    image = cv.resize(image, (h_in, w_in), cv.INTER_CUBIC)

    x_test = np.empty((1, h_in, w_in, 1), dtype=np.float32)
    x_test[0, :, :, 0] = image / 255.

    X_colorized = model.predict(x_test)
    X_colorized = X_colorized.reshape((h_out * w_out, nb_q))

    X_colorized = np.exp(np.log(X_colorized + epsilon) / T)
    X_colorized = X_colorized / np.sum(X_colorized, 1)[:, np.newaxis]

    q_a = q_ab[:, 0].reshape((1, 313))
    q_b = q_ab[:, 1].reshape((1, 313))

    X_a = np.sum(X_colorized * q_a, 1).reshape((h_out, w_out))
    X_b = np.sum(X_colorized * q_b, 1).reshape((h_out, w_out))

    X_a = cv.resize(X_a, (img_cols, img_rows), cv.INTER_CUBIC)
    X_b = cv.resize(X_b, (img_cols, img_rows), cv.INTER_CUBIC)

    X_a = X_a + 128
    X_b = X_b + 128

    out_lab = np.zeros((img_rows, img_cols, 3), dtype=np.int32)
    out_lab[:, :, 0] = L
    out_lab[:, :, 1] = X_a
    out_lab[:, :, 2] = X_b

    out_lab = out_lab.astype(np.uint8)
    out_bgr = cv.cvtColor(out_lab, cv.COLOR_LAB2BGR)

    out_bgr = out_bgr.astype(np.uint8)
    out_bgr = cv.cvtColor(out_bgr, cv.COLOR_BGR2RGB)

    out_bgr = Image.fromarray(out_bgr)

    return out_bgr
