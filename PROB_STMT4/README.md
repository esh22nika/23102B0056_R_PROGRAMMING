# Image Recognition and Classification Using R

## Project Objective

The objective of this project is to implement an image recognition and classification system using **R Programming** and deep learning. The project demonstrates how image data can be loaded, preprocessed, and classified using a neural network model.

## Problem Statement

Image classification is the process of assigning an input image to a predefined category. In this project, images are processed using R and a neural network model is trained to recognize different image classes.

The project follows the implementation demonstrated in the prescribed tutorial and applies concepts such as image preprocessing, data preparation, model building, training, and evaluation.

## Technologies Used

* R Programming
* RStudio
* Keras
* TensorFlow
* EBImage
* Neural Networks / Deep Learning

## R Packages Used

```r
library(EBImage)
library(keras)
library(tensorflow)
```

## Dataset

The project uses a collection of labelled images organized according to their respective classes.

The images are:

* Loaded from the dataset directory
* Resized to a common dimension
* Converted into numerical arrays
* Prepared for training
* Associated with their corresponding class labels

## Project Workflow

1. Load the required R libraries.
2. Read the image files from the dataset.
3. Explore and inspect the images.
4. Resize the images to a fixed size.
5. Convert the images into numerical data.
6. Prepare the training and testing data.
7. Encode the class labels.
8. Build the neural network model using Keras.
9. Compile the model.
10. Train the model using the prepared dataset.
11. Evaluate the trained model.
12. Test the model on images and generate predictions.

## Model Implementation

The Keras framework is used to construct the neural network.

The general workflow is:

```text
Images
   ↓
Image Loading
   ↓
Image Preprocessing
   ↓
Resize / Reshape
   ↓
Label Encoding
   ↓
Neural Network
   ↓
Model Training
   ↓
Model Evaluation
   ↓
Prediction
```

## How to Run the Project

### 1. Install R and RStudio

Install R and RStudio on the system.

### 2. Install Required Packages

Run the following commands in R:

```r
install.packages("keras")
install.packages("tensorflow")
```

Install/load the required packages:

```r
library(EBImage)
library(keras)
library(tensorflow)
```

### 3. Prepare the Dataset

Place the image dataset in the appropriate project directory and ensure that the image paths used in the R script are correct.

### 4. Run the R Script

Open the project in RStudio and execute the R script from beginning to end.

### 5. Verify the Output

Check the generated training results, model evaluation, and classification predictions.

## Results

The implemented model successfully performs the image classification workflow by:

* Reading image data
* Preprocessing images
* Training a neural network
* Evaluating the trained model
* Predicting image classes

Screenshots of the successful execution and generated results are included below.

## Screenshots

### Dataset / Input Images

*Add screenshot here.*

### Model Training

*Add screenshot of the training output here.*

### Prediction / Classification Result

*Add screenshot here.*

## Git and GitHub

Git was used for version control throughout the project.

Example commands:

```bash
git init
git add .
git commit -m "Initial project setup"

git add .
git commit -m "Implemented image preprocessing"

git add .
git commit -m "Added neural network model"

git add .
git commit -m "Added results and documentation"

git branch -M main
git remote add origin <YOUR_GITHUB_REPOSITORY_URL>
git push -u origin main
```

Meaningful commits were maintained at different stages instead of uploading the complete project in a single commit.

## Conclusion

This project demonstrates an end-to-end image classification workflow using **R, Keras, and TensorFlow**. It provides practical experience in image preprocessing, neural network implementation, model training, evaluation, and prediction. Git and GitHub were also used to maintain, document, and publish the project.

## Reference

Prescribed tutorial:
https://www.youtube.com/watch?v=iExh0qj2Ouo

Assignment reference: Lab Problem 4 – R Project Implementation and Version Control Using GitHub.

