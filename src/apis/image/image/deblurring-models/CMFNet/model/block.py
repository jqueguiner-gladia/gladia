from logging import getLogger

import torch
import torch.nn as nn

logger = getLogger(__name__)

##########################################################################
def conv(
    in_channels: int,
    out_channels: int,
    kernel_size: int,
    bias: bool = False,
    stride: int = 1,
) -> nn.Conv2d:
    """
    Convolutional layer

    Args:
        in_channels (int): Number of input channels.
        out_channels (int): Number of output channels.
        kernel_size (int): Size of the convolving kernel.
        bias (bool, optional): If set to False, the layer will not learn an additive bias. (default: True)
        stride (int, optional): Stride of the convolution. (default: 1)

    Returns:
        nn.Conv2d: Convolutional layer.
    """
    layer = nn.Conv2d(
        in_channels,
        out_channels,
        kernel_size,
        padding=(kernel_size // 2),
        bias=bias,
        stride=stride,
    )
    return layer


def conv3x3(in_chn: int, out_chn: int, bias=True) -> nn.Conv2d:
    """
    Convolutional layer

    Args:
        in_channels (int): Number of input channels.
        out_channels (int): Number of output channels.
        bias (bool, optional): If set to False, the layer will not learn an additive bias. (default: True)

    Returns:
        nn.Conv2d: Convolutional layer.
    """
    layer = nn.Conv2d(in_chn, out_chn, kernel_size=3, stride=1, padding=1, bias=bias)
    return layer


def conv_down(in_chn: int, out_chn: int, bias: int = False) -> nn.Conv2d:
    """
    Convolutional layer

    Args:
        in_channels (int): Number of input channels.
        out_channels (int): Number of output channels.
        bias (bool, optional): If set to False, the layer will not learn an additive bias. (default: True)

    Returns:
        nn.Conv2d: Convolutional layer.
    """
    layer = nn.Conv2d(in_chn, out_chn, kernel_size=4, stride=2, padding=1, bias=bias)
    return layer


##########################################################################
## Supervised Attention Module (SAM)
class SAM(nn.Module):
    def __init__(self, n_feat, kernel_size, bias):
        super(SAM, self).__init__()
        self.conv1 = conv(n_feat, n_feat, kernel_size, bias=bias)
        self.conv2 = conv(n_feat, 3, kernel_size, bias=bias)
        self.conv3 = conv(3, n_feat, kernel_size, bias=bias)

    def forward(self, x, x_img):
        x1 = self.conv1(x)
        img = self.conv2(x) + x_img
        x2 = torch.sigmoid(self.conv3(img))
        x1 = x1 * x2
        x1 = x1 + x
        return x1, img


##########################################################################
## Spatial Attention
class SALayer(nn.Module):
    def __init__(self, kernel_size=7):
        super(SALayer, self).__init__()
        self.conv1 = nn.Conv2d(2, 1, kernel_size, padding=kernel_size // 2, bias=False)
        self.sigmoid = nn.Sigmoid()

    def forward(self, x):
        avg_out = torch.mean(x, dim=1, keepdim=True)
        max_out, _ = torch.max(x, dim=1, keepdim=True)
        y = torch.cat([avg_out, max_out], dim=1)
        y = self.conv1(y)
        y = self.sigmoid(y)
        return x * y


# Spatial Attention Block (SAB)
class SAB(nn.Module):
    def __init__(self, n_feat, kernel_size, reduction, bias, act):
        super(SAB, self).__init__()
        modules_body = [
            conv(n_feat, n_feat, kernel_size, bias=bias),
            act,
            conv(n_feat, n_feat, kernel_size, bias=bias),
        ]
        self.body = nn.Sequential(*modules_body)
        self.SA = SALayer(kernel_size=7)

    def forward(self, x):
        res = self.body(x)
        res = self.SA(res)
        res += x
        return res


##########################################################################
## Pixel Attention
class PALayer(nn.Module):
    def __init__(self, channel, reduction=16, bias=False):
        super(PALayer, self).__init__()
        self.pa = nn.Sequential(
            nn.Conv2d(channel, channel // reduction, 1, padding=0, bias=bias),
            nn.ReLU(inplace=True),
            nn.Conv2d(
                channel // reduction, channel, 1, padding=0, bias=bias
            ),  # channel <-> 1
            nn.Sigmoid(),
        )

    def forward(self, x):
        y = self.pa(x)
        return x * y


## Pixel Attention Block (PAB)
class PAB(nn.Module):
    def __init__(self, n_feat, kernel_size, reduction, bias, act):
        super(PAB, self).__init__()
        modules_body = [
            conv(n_feat, n_feat, kernel_size, bias=bias),
            act,
            conv(n_feat, n_feat, kernel_size, bias=bias),
        ]
        self.PA = PALayer(n_feat, reduction, bias=bias)
        self.body = nn.Sequential(*modules_body)

    def forward(self, x):
        res = self.body(x)
        res = self.PA(res)
        res += x
        return res


##########################################################################
## Channel Attention Layer
class CALayer(nn.Module):
    def __init__(self, channel, reduction=16, bias=False):
        super(CALayer, self).__init__()
        # global average pooling: feature --> point
        self.avg_pool = nn.AdaptiveAvgPool2d(1)
        # feature channel downscale and upscale --> channel weight
        self.conv_du = nn.Sequential(
            nn.Conv2d(channel, channel // reduction, 1, padding=0, bias=bias),
            nn.ReLU(inplace=True),
            nn.Conv2d(channel // reduction, channel, 1, padding=0, bias=bias),
            nn.Sigmoid(),
        )

    def forward(self, x):
        y = self.avg_pool(x)
        y = self.conv_du(y)
        return x * y


## Channel Attention Block (CAB)
class CAB(nn.Module):
    def __init__(self, n_feat, kernel_size, reduction, bias, act):
        super(CAB, self).__init__()
        modules_body = [
            conv(n_feat, n_feat, kernel_size, bias=bias),
            act,
            conv(n_feat, n_feat, kernel_size, bias=bias),
        ]

        self.CA = CALayer(n_feat, reduction, bias=bias)
        self.body = nn.Sequential(*modules_body)

    def forward(self, x):
        res = self.body(x)
        res = self.CA(res)
        res += x
        return res


if __name__ == "__main__":
    import time

    from thop import profile

<<<<<<< HEAD
    layer = PAB(64, 3, 4, False, nn.PReLU())

=======
    # layer = CAB(64, 3, 4, False, nn.PReLU())
    layer = PAB(64, 3, 4, False, nn.PReLU())
    # layer = SAB(64, 3, 4, False, nn.PReLU())
>>>>>>> 88c137d7b884df8f495273040b4d07623cc3d53d
    for idx, m in enumerate(layer.modules()):
        print(idx, "-", m)
    s = time.time()

    rgb = torch.ones(1, 64, 256, 256, dtype=torch.float, requires_grad=False)
    out = layer(rgb)
    flops, params = profile(layer, inputs=(rgb,))
<<<<<<< HEAD
    logger.info(f"parameters: {params}")
    logger.info(f"flops {flops}")
=======
    logger.info("parameters:", params)
    logger.info("flops", flops)
>>>>>>> 88c137d7b884df8f495273040b4d07623cc3d53d
    logger.info("time: {:.4f}ms".format((time.time() - s) * 10))
