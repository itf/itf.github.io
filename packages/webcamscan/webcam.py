import pygame
import pygame.camera
from pygame.locals import *
import sys
from PIL import Image
import os
from time import time
import numpy as np


DEVICE = "/dev/video1"
SIZE = (1280, 720)

#https://www.pyimagesearch.com/2015/09/07/blur-detection-with-opencv/

#try:
if True:
    import cv2
    has_cv = True

    def variance_of_laplacian(image):
        # compute the Laplacian of the image and then return the focus
        # measure, which is simply the variance of the Laplacian
        ans =  cv2.Laplacian(image, cv2.CV_64F).var()

        print(ans)
        return cv2.Laplacian(image, cv2.CV_64F).var()

    def choose_best(image1, image2):
        if variance_of_laplacian(pygame_to_cvimage(image1))>variance_of_laplacian(pygame_to_cvimage(image2)):
            return image1
        return image2

    #https://gist.github.com/jpanganiban/3844261
    def pygame_to_cvimage(surface):
        """Convert a pygame surface into a cv image"""
        ## convert to pillow
        pil_string_image = pygame.image.tostring(surface, "RGB", False)
        pil_im = Image.frombytes("RGB", SIZE, pil_string_image)

        numpy_image = np.array(pil_im)
        opencv_image = cv2.cvtColor(numpy_image, cv2.COLOR_RGB2BGR) 
        gray = cv2.cvtColor(opencv_image, cv2.COLOR_BGR2GRAY)
        return gray

#except ImportError:
else:
    has_cv = False

    def choose_best(image1, image2):
        return image2











#based on https://gist.github.com/snim2/255151
#and https://www.pygame.org/docs/tut/CameraIntro.html


if __name__ == "__main__":
    file_name = str(sys.argv[1]) 
    pygame.init()
    pygame.camera.init()

    cam = pygame.camera.Camera(DEVICE, SIZE)
    cam.start()

    display = pygame.display.set_mode(SIZE, 0)
    screen = pygame.surface.Surface(SIZE, 0, display)
    begin_time = time()

    #Needed for the camera to adjust brightness and focus
    image = cam.get_image()
    while time()-begin_time < 1.1:
        screen = cam.get_image()
        display.blit(screen, (0,0))
        pygame.display.flip()
        image = choose_best(image, screen)
    
    cam.stop()

    pil_string_image = pygame.image.tostring(image,"RGB",False)
    im = Image.frombytes("RGB", SIZE ,pil_string_image)
    
    out_file = open(file_name, 'wb')
    im.save(out_file, "JPEG")


    #from: http://discourse.techart.online/t/pil-wait-for-image-save/3994/6

    out_file.flush()
    os.fsync(out_file)
    out_file.close()

    pygame.quit()
