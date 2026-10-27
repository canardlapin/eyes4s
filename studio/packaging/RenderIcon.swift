// Copyright 2026 canardlapin. Licensed under the Apache License, Version 2.0.
// Headless vector rendering. Colours are supplied from Studio Tokens.scala.
import AppKit

let destination = URL(fileURLWithPath: CommandLine.arguments[1], isDirectory: true)
func colour(_ index: Int) -> NSColor {
    let hex = UInt32(CommandLine.arguments[index], radix: 16)!
    return NSColor(srgbRed: CGFloat((hex >> 16) & 255) / 255,
                   green: CGFloat((hex >> 8) & 255) / 255,
                   blue: CGFloat(hex & 255) / 255, alpha: 1)
}
let background = colour(2)
let ink = colour(3)
let center = colour(4)
for size in [16, 32, 128, 256, 512] {
    for scale in [1, 2] {
        let pixels = size * scale
        let image = NSBitmapImageRep(bitmapDataPlanes: nil, pixelsWide: pixels, pixelsHigh: pixels,
            bitsPerSample: 8, samplesPerPixel: 4, hasAlpha: true, isPlanar: false,
            colorSpaceName: .deviceRGB, bytesPerRow: 0, bitsPerPixel: 0)!
        NSGraphicsContext.saveGraphicsState()
        NSGraphicsContext.current = NSGraphicsContext(bitmapImageRep: image)
        let transform = AffineTransform(scale: CGFloat(pixels) / 1024)
        (transform as NSAffineTransform).concat()
        background.setFill()
        NSBezierPath(roundedRect: NSRect(x: 64, y: 64, width: 896, height: 896),
                     xRadius: 190, yRadius: 190).fill()
        let eye = NSBezierPath()
        eye.move(to: NSPoint(x: 184, y: 512))
        eye.curve(to: NSPoint(x: 840, y: 512),
                  controlPoint1: NSPoint(x: 360, y: 770), controlPoint2: NSPoint(x: 664, y: 770))
        eye.curve(to: NSPoint(x: 184, y: 512),
                  controlPoint1: NSPoint(x: 664, y: 254), controlPoint2: NSPoint(x: 360, y: 254))
        eye.close()
        ink.setStroke()
        eye.lineWidth = 56
        eye.lineJoinStyle = .round
        eye.stroke()
        center.setFill()
        NSBezierPath(ovalIn: NSRect(x: 402, y: 402, width: 220, height: 220)).fill()
        NSGraphicsContext.restoreGraphicsState()
        let suffix = scale == 2 ? "@2x" : ""
        try image.representation(using: .png, properties: [:])!.write(
            to: destination.appendingPathComponent("icon_\(size)x\(size)\(suffix).png"))
    }
}
