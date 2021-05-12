Há duas estruturas de dados principais a serem consideradas em `ImgComonad`:
`Image` e `FocusedImage`. `Image a` representa um vetor bidimensional com
elementos do tipo `a`. `FocusedImage a` representa uma posição específica em uma
`Image a`. 

O método básico de utilização da biblioteca é, primeiramente, carregar ou gerar
uma `Image` com o tipo de pixel desejado, e então usar `focus` para transformá-la
em uma `FocusedImage`:

    img <- readImage "filename.png"
    let fimg = focus img

Para processar a imagem, usa-se `fmap` ou `extend`, dependendo de se o
processamento desejado precisa só do pixel origem ou dos vizinhos também:

    let img_but_floats = fmap (\p -> (fromIntegral p) / 256) fimg
    let img_but_badly_blurred = extend (\i -> sum [ pixel i 0 x y | x <- [-1..1], y <- [-1..1] ] / 9) fimg

Note que `pixel` usa coordenadas em relação ao pixel sendo considerado, não em
relação ao sistema de coordenadas da imagem. O parâmetro da função passada para
`extend` é uma `FocusedImage`, mas com o foco ajustado para o pixel sendo
considerado.

O tipo `Image a` tem as seguintes funções associadas:

    newImage :: Int -> Int -> (Int -> Int -> a) -> Image a
    newImage w h f
        Gera uma imagem de tamanho (w, h) a partir do resultado da função f para
        cada par de coordenadas.
    ipixel :: Image a -> a -> Int -> Int -> a
    ipixel image default x y
        Lê o pixel (x, y) da imagem image.
        Se as coordenadas não estiverem na imagem, retorna default.
    iinside :: Image a -> Int -> Int -> Bool
    iinside image x y
        Retorna se as coordenadas (x, y) estiverem dentro da imagem.
    fmap :: (a -> b) -> Image a -> Image b
    fmap f image
        Aplica a função f a cada pixel da imagem image e retorna a imagem
        resultante.
    iZipWith :: (a -> b -> c) -> Image a -> Image b -> Image c
    iZipWith f a b
        Aplica a função f a cada par de elementos em a e b e retorna a imagem
        resultante.
    readImage :: FilePath -> IO (Image Word8)
    readImage path
        Lê uma imagem do sistema de arquivos e converte-a para grayscale.
    writePng :: FilePath -> Image Word8 -> IO ()
    writePng path image
        Escreve uma imagem em formato PNG.
    writeGifAnim :: FilePath -> [Image Word8] -> IO ()
    writeGifAnim path images
        Escreve uma lista de imagens como uma animação GIF.

O tipo `FocusedImage a` tem as seguintes funções associadas:

    focus :: Image a -> FocusedImage a
    focus image
        Cria um foco na posição (0, 0) da imagem.
    unfocus :: FocusedImage a -> Image a
    unfocus fimage
        Remove o foco da imagem.
    pixel :: FocusedImage a -> a -> Int -> Int -> a
    pixel fimage default x y
        Lê o pixel (x, y) da imagem fimage, em relação ao foco.
        Se as coordenadas não estiverem na imagem, retorna default.
    inside :: FocusedImage a -> Int -> Int -> Bool
    inside fimage x y
        Retorna se as coordenadas (x, y) em relação ao foco estiverem dentro da
        imagem.
    fmap :: (a -> b) -> FocusedImage a -> FocusedImage b
    fmap f fimage
        Aplica a função f a cada pixel da imagem fimage e retorna a imagem
        resultante, mantendo o foco na mesma posição.
    zipWith :: (a -> b -> c) -> FocusedImage a -> FocusedImage b -> FocusedImage c
    zipWith f a b
        Aplica a função f a cada par de elementos em a e b e retorna a imagem
        resultante, independentemente dos focos.
    extract :: FocusedImage a -> a
    extract fimage
        Retorna o valor do pixel no foco.
    extend :: (FocusedImage a -> b) -> FocusedImage a -> FocusedImage b
    extend f fimage
        Aplica a função f à imagem para cada ponto focal possível, e retorna a
        imagem resultante.

