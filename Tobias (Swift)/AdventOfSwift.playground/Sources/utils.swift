import Cocoa

public func getInput(name: String) -> String
{
  let filePath = NSBundle.mainBundle().pathForResource("Input" + name, ofType: "txt")
  let contentData = NSFileManager.defaultManager().contentsAtPath(filePath!)
  return (NSString(data: contentData!, encoding: NSUTF8StringEncoding) as? String)!
}
