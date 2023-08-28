{-# LANGUAGE RecordWildCards #-}

module Charts (plotChart) where
import Data.Csv (decodeByName)

import qualified Data.ByteString.Lazy as BL (readFile)
import Data.Foldable (toList)
import Graphics.Rendering.Chart.Backend.Diagrams
    ( FileFormat (SVG),
      FileOptions (FileOptions),
      loadSansSerifFonts,
      renderableToFile,
    )
import Graphics.Rendering.Chart.Easy
    ( opaque,
      white,
      gray,
      cyan,
      green,
      layout_plots,
      layout_title,
      (.~),
      solidFillStyle,
      slayouts_layouts,
      plot_lines_values,
      plot_lines_title,
      plot_lines_style,
      plot_candle_width,
      plot_candle_values,
      plot_candle_title,
      plot_candle_tick_length,
      plot_candle_rise_fill_style,
      plot_candle_line_style,
      plot_candle_fill,
      plot_candle_fall_fill_style,
      plot_bars_values,
      plot_bars_titles,
      plot_bars_item_styles,
      plotBars,
      line_width,
      line_color,
      ToPlot(toPlot),
      StackedLayout(StackedLayout),
      Default(def),
      Candle(Candle),
      ToRenderable(toRenderable) )
import QuoteData (QuoteData (..))

plotChart :: Foldable t => String -> t QuoteData -> FilePath -> IO ()
plotChart title quotes fname = do
    _ <- renderableToFile fileOptions fname (toRenderable chart)
    pure ()
    where
        fileOptions = FileOptions (1600, 1400) SVG loadSansSerifFonts

        (candles, closings, volumes) =
            unzip3 $
                [ ( Candle day low open 0 close high
                  , (day, close)
                  , (day, [volume])
                  )
                | QuoteData {..} <- toList quotes
                ]

        chart =
            slayouts_layouts
                .~ [ StackedLayout candlesLayout
                   , StackedLayout volumesLayout
                   ]
                $ def

        candlesLayout =
            layout_title .~ title
                $ layout_plots
                    .~ [ toPlot $ qline "Close" closings green
                       , toPlot $ candle "Candle" candles cyan
                       ]
                $ def

        volumesLayout =
            layout_plots .~ [plotBars $ bars "Volume" volumes gray] $
                def

        candle label values color =
            plot_candle_line_style .~ lineStyle 1 gray $
                plot_candle_fill .~ True $
                    plot_candle_rise_fill_style .~ fillStyle white $
                        plot_candle_fall_fill_style .~ fillStyle color $
                            plot_candle_tick_length .~ 0 $
                                plot_candle_width .~ 3 $
                                    plot_candle_values .~ values $
                                        plot_candle_title .~ label $
                                            def

        qline label values color =
            plot_lines_style .~ lineStyle 1 color $
                plot_lines_values .~ [values] $
                    plot_lines_title .~ label $
                        def

        bars label values color =
            plot_bars_titles .~ [label] $
                plot_bars_values .~ values $
                    plot_bars_item_styles .~ [(fillStyle color, Nothing)] $
                        def

        fillStyle color = solidFillStyle (opaque color)

        lineStyle n color =
            line_width .~ n $
                line_color .~ opaque color $
                    def

readQuotes :: FilePath -> IO [QuoteData]
readQuotes fpath = do
    csvData <- BL.readFile fpath
    case decodeByName csvData of
        Left err -> error err
        Right (_, quotes) -> pure (toList quotes)