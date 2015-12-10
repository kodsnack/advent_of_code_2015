/*
--- Day 2: I Was Told There Would Be No Math ---

The elves are running low on wrapping paper, and so they need to submit an order for more. They have a list of the dimensions (length l, width w, and height h) of each present, and only want to order exactly as much as they need.

Fortunately, every present is a box (a perfect right rectangular prism), which makes calculating the required wrapping paper for each gift a little easier: find the surface area of the box, which is 2*l*w + 2*w*h + 2*h*l. The elves also need a little extra paper for each present: the area of the smallest side.
*/

func getDimensionsFromString(str: String) -> (length: Int, width: Int, height: Int)
{
  // there must be a better way to do this
  let dimensions = str.characters.split{$0 == "x"}.map(String.init).map{Int($0)!}
  return (dimensions[0], dimensions[1], dimensions[2])
}

func getWrappingPaperVolume(dimensions: String) -> Int
{
  let d = getDimensionsFromString(dimensions)
  // since we need the smallest side as slack, calculate each side independently
  let side1 = d.length * d.width
  let side2 = d.width * d.height
  let side3 = d.height * d.length

  let total = (2 * (side1 + side2 + side3)) + min(side1, side2, side3)

  return total
}



// A present with dimensions 2x3x4 requires 2*6 + 2*12 + 2*8 = 52 square feet of wrapping paper plus 6 square feet of slack, for a total of 58 square feet.
getWrappingPaperVolume("2x3x4") == 58

// A present with dimensions 1x1x10 requires 2*1 + 2*10 + 2*10 = 42 square feet of wrapping paper plus 1 square foot of slack, for a total of 43 square feet.
getWrappingPaperVolume("1x1x10") == 43

let volumeInput = getInput("2")
var totalVolume = 0
volumeInput.enumerateLines { (line, stop) -> () in
  totalVolume += getWrappingPaperVolume(line)
}

totalVolume


/*
--- Part Two ---

The elves are also running low on ribbon. Ribbon is all the same width, so they only have to worry about the length they need to order, which they would again like to be exact.

The ribbon required to wrap a present is the shortest distance around its sides, or the smallest perimeter of any one face. Each present also requires a bow made out of ribbon as well; the feet of ribbon required for the perfect bow is equal to the cubic feet of volume of the present. Don't ask how they tie the bow, though; they'll never tell.
*/

func getRibbonLengthForPackage(dimensions: String) -> Int
{
  let d = getDimensionsFromString(dimensions)
  let cubic = d.length * d.width * d.height

  let s1 = d.length + d.width
  let s2 = d.width + d.height
  let s3 = d.length + d.height

  return cubic + (2 * min(s1, s2, s3))
}

// A present with dimensions 2x3x4 requires 2+2+3+3 = 10 feet of ribbon to wrap the present plus 2*3*4 = 24 feet of ribbon for the bow, for a total of 34 feet.
getRibbonLengthForPackage("2x3x4") == 34

// A present with dimensions 1x1x10 requires 1+1+1+1 = 4 feet of ribbon to wrap the present plus 1*1*10 = 10 feet of ribbon for the bow, for a total of 14 feet.
getRibbonLengthForPackage("1x1x10") == 14

let ribbonInput = getInput("2")
var totalLength = 0
ribbonInput.enumerateLines { (line, stop) -> () in
  totalLength += getRibbonLengthForPackage(line)
}

totalLength