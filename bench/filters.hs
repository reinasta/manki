{-# LANGUAGE BangPatterns #-}
import Criterion.Main
import Control.DeepSeq (deepseq)

import Indices (updateIndices,rewriteIndices)
import Samples (csvAST1,csvAST2,csvAST3)



-- Our benchmark harness.
main = defaultMain [
  bgroup "updateIndices" [ bench "small"  $ wnf updateIndices csvAST1
                         , bench "medium"  $ whnf updateIndices csvAST2
                         , bench "large"  $ whnf updateIndices csvAST3
                         ]
  bgroup "rewriteIndices" [ bench "small"  $ whnf rewriteIndices csvAST1
                          , bench "medium"  $ whnf rewriteIndices csvAST2
                          , bench "large"  $ whnf rewriteIndices csvAST3
                          ]

  ]


