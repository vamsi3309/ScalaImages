package ScalaImages

import java.awt.image.BufferedImage
import java.io.File
import javax.imageio.ImageIO
import java.awt.{Color, color}
import javax.swing.ImageIcon
import javax.swing.JFrame
import javax.swing.JLabel
import java.awt.FlowLayout
import scalation.linalgebra.{MatrixI, VectorI}


class Image(path: String) {

  /*def getBuffImage() returns the buffered image object on the path.*/
  private def getBuffImage(): java.awt.image.BufferedImage = {
    val image = new File(this.path)
    ImageIO.read(image)
  }

  var buffImage: BufferedImage = getBuffImage()

  /*def getRgbArray method returns a array of RGB values of size image.width * image.height*/
  def getRgbArray():Array[Int] = {
    val image = this.buffImage
    var pixels = new Array[Int](image.getHeight*image.getWidth)
    image.getRGB(0,0,image.getWidth,image.getHeight,pixels,0,image.getWidth)
  }

  /*def getRed method returns vectorI with values red channel from the image*/
  def getRed(): scalation.linalgebra.VectorI = {
    val rgb = this.getRgbArray()
    var red = rgb.map(x => new Color(x).getRed)
    new VectorI(red.length,red)
  }

  /*def getGreen method returns vectorI with values green channel from the image*/
  def getGreen(): scalation.linalgebra.VectorI = {
    val rgb = this.getRgbArray()
    var green = rgb.map(x => new Color(x).getGreen)
    new VectorI(green.length,green)
  }

  /*def getBlue method returns vectorI with values blue channel from the image*/
  def getBlue(): scalation.linalgebra.VectorI = {
    val rgb = this.getRgbArray()
    var blue = rgb.map(x => new Color(x).getBlue)
    new VectorI(blue.length,blue)
  }

  /*def getAlpha method returns vectorI with values alpha channel from the image*/
  def getAlpha(): scalation.linalgebra.VectorI = {
    val rgb = this.getRgbArray()
    var alpha = rgb.map(x => new Color(x).getAlpha)
    new VectorI(alpha.length,alpha)
  }

  /*Returns height of the image*/
  def height(): Int = {
    this.buffImage.getHeight
  }

  /*Returns width of the image*/
  def width(): Int = {
    this.buffImage.getWidth
  }

  /*Converts the image into a grey scale image and return MatrixI object*/
  def toGrey(): MatrixI = {
    var red = getRed().*=(21)./(100)
    var blue = getBlue().*=(8)./(100)
    var green = getGreen().*=(71)./(100)
    val bwarr = red.+(green).+(blue)
    toMatrix(bwarr,this.height(),this.width())
  }

  /*Converts a vector to matrix*/
  def toMatrix(vec: VectorI,height: Int,width: Int): MatrixI = {
    var matI = new MatrixI(height,width)
    for ( i <- (0 until height))
        matI.set(i, vec.apply(i * width until (i + 1) * width))
    matI
  }

  /*Converts a array to matrix*/
  def toMatrix(arr: Array[Int],height: Int,width: Int): MatrixI = {
    var matI = new MatrixI(height,width)
    var vecI = new VectorI(arr.length,arr)
    for ( i <- (0 until height))
        matI.set(i, vecI.apply(i * width until (i + 1) * width))
    matI
  }

  /*Converts a matrixI to buffered image*/
  def toBuffImage(mat: MatrixI): BufferedImage = {
    var resBuffImage = new BufferedImage(mat.dim2,mat.dim1,BufferedImage.TYPE_BYTE_GRAY)
    for (i <- (0 until mat.dim1))
      for (j <- (0 until mat.dim2)){
        var rbg = mat.apply(i,j)
        var color = new Color(rbg,rbg,rbg)
        resBuffImage.setRGB(j,i,color.getRGB)
      }
    resBuffImage
  }

  /*Saves the image to file for a given path and format*/
  def toFile(path: String, format: String): Unit = {
    ImageIO.write(this.buffImage,format,new File(path))
  }

  /*Display the images*/
  def displayImage(): Unit = {
    val icon = new ImageIcon(this.buffImage)
    val frame = new JFrame
    frame.setLayout(new FlowLayout)
    frame.setSize(this.buffImage.getWidth, this.buffImage.getHeight)
    val lbl = new JLabel
    lbl.setIcon(icon)
    frame.add(lbl)
    frame.setVisible(true)
    frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE)
  }

  /*def deNoise method is for denoising the image. This is based on local neighbour mean method*/
  private def deNoise(): Unit ={
    val bufimg = buffImage
    val redmat = toMatrix(getRed(),bufimg.getHeight,bufimg.getWidth)
    val greenmat = toMatrix(getGreen(),bufimg.getHeight,bufimg.getWidth)
    val bluemat = toMatrix(getBlue(),bufimg.getHeight,bufimg.getWidth)
    for (i <- (1 until this.height()-1))
      for(j <- (1 until this.width()-1)){
        val red = (redmat.apply(i,j) + redmat.apply(i-1,j) + redmat.apply(i+1,j) + redmat.apply(i,j-1) + redmat.apply(i,j+1))/5
        val green = (greenmat.apply(i,j) + greenmat.apply(i-1,j) + greenmat.apply(i+1,j)
          + greenmat.apply(i,j-1) + greenmat.apply(i,j+1))/5
        val blue = (bluemat.apply(i,j) + bluemat.apply(i-1,j) + bluemat.apply(i+1,j) + bluemat.apply(i,j-1) + bluemat.apply(i,j+1))/5
        bufimg.setRGB(j,i,new Color(red,green,blue).getRGB)
      }
  }

  /*deNoising method calls deNoise() method repeatedly to reduce the noise of the image */
  def deNoising(): Unit ={
    deNoise()
    deNoise()
    deNoise()
    deNoise()
  }


}

object sampleImageIO extends App{
    val img = new Image("/home/vamsi/Downloads/balloons_noisy.png")
    println("Red Values[0]",img.getRed().apply(0))
    println("Green Values[0]",img.getGreen().apply(0))
    println("Blue Values[0]",img.getBlue().apply(0))
    println("Alpha Values[0]",img.getAlpha().apply(0))
    img.deNoising()
    img.displayImage()
}

