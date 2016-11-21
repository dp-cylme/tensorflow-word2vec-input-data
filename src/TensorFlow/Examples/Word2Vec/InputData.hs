
module TensorFlow.Examples.Word2Vec.InputData
  (trainingTextData)
  where

import Paths_tensorflow_word2vec_input_data (getDataFileName)


trainingTextData :: IO FilePath
trainingTextData = getDataFileName "text8.zip"
