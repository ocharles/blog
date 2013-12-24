{-# LANGUAGE NoMonomorphismRestriction #-}
import Control.Arrow
import Control.Lens
import Data.Default.Class
import Data.Colour
import Data.Colour.SRGB
import Graphics.Rendering.Chart
import Graphics.Rendering.Chart.Backend.Cairo

data BlogPost = BlogPost { blogPostLibrary :: String
                           , blogPostUpvotes :: Int
                           , blogPostDownvotes :: Int
                           , blogPostComments :: Int
                           }

blogPosts :: [BlogPost]
blogPosts =
  [ BlogPost "2013 in Review" 73 13 11
  , BlogPost "linear" 75 8 43
  , BlogPost "tasty" 79 8 4
  , BlogPost "extensible-effects" 63 6 25
  , BlogPost "scotty" 80 3 16
  , BlogPost "persistent/esqueleto" 55 3 14
  , BlogPost "threepenny-gui" 75 8 5
  , BlogPost "data-memocombinators" 63 6 18
  , BlogPost "sbv" 64 10 21
  , BlogPost "gloss" 59 8 10
  , BlogPost "heist" 48 9 28
  , BlogPost "pandoc" 75 6 14
  , BlogPost "async" 65 8 11
  , BlogPost "acid-state" 91 11 15
  , BlogPost "time" 55 7 30
  , BlogPost "repa" 71 11 17
  , BlogPost "unordered-containers" 51 8 12
  , BlogPost "doctest" 69 3 3
  , BlogPost "websockets" 69 6 8
  , BlogPost "web-routes-boomerang" 35 6 6
  , BlogPost "contravariant" 60 4 17
  , BlogPost "profunctors" 41 2 9
  , BlogPost "fay" 35 3 27
  ]

upDownPerPost :: PlotBars Int Int
upDownPerPost =
  def & plot_bars_style .~ BarsStacked
      & plot_bars_item_styles .~ [ (FillStyleSolid $ withOpacity (sRGB24 255 0 0) 100, Nothing)
                                 , (FillStyleSolid $ withOpacity (sRGB24 0 255 0) 100, Nothing)
                                 ]
      & plot_bars_titles .~ [ "Downvotes", "Upvotes" ]
      & plot_bars_values .~ dataPoints
 where
  dataPoints =
   zip [1..] $ map (sequence [blogPostDownvotes, blogPostUpvotes]) blogPosts

upVotesAgainstComments :: PlotPoints Int Int
upVotesAgainstComments =
  def & plot_points_title .~ "Upvotes vs. Comments"
      & plot_points_values .~ dataPoints
      & plot_points_style .~ (def & point_radius .~ 3)
 where
  dataPoints = map (blogPostUpvotes &&& blogPostComments) blogPosts

upVotePieChart :: PieChart
upVotePieChart =
  def & pie_data .~ dataPoints
 where
  dataPoints = map (\post -> def & pitem_label .~ (blogPostLibrary post)
                                 & pitem_value .~ (fromIntegral $ blogPostUpvotes post))
                   blogPosts

main =
  let upDown = def & layout_plots .~ [plotBars upDownPerPost]
      upVotesComments = def & layout_plots .~ [toPlot upVotesAgainstComments]
      upVotesPie = def & pie_plot .~ upVotePieChart
  in do
       renderableToFile def (toRenderable upDown) "up-down.png"
       renderableToFile def (toRenderable upVotesPie) "upvotes-pie.png"
       renderableToFile def (toRenderable upVotesComments) "upvotes-comments.png"
