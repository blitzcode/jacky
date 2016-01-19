
# Jacky

This program is a very much work-in-progress (about 50-75% done) Twitter 'newsreader'. Basically, a Twitter client designed only for consumption of tweets, my usage model for the service. It's not usable yet, but large parts of the network logic, image handling and custom OpenGL-based UI are done. I'm sharing this here now as many of the individual components are stable, fast and useful outside the context of this program.

![Main Debug](https://raw.github.com/blitzcode/jacky/master/img/main_debug.png)

# Building

A Makefile based build system (`src/old_Makefile`) was used for most of the development, but later replaced by a Cabal/Stack based one. What makes building this project a bit complex is the configure step of the `jacky.cabal` invoking the FreeType 2 Makefile in `src/freetype2/`. There's a also some C code (FT2 wrappers) build by Cabal directly.

# Modules

Incomplete list of the various modules which make up the client, including a brief explanation of their function, algorithm and data structure choices.

### App

Application logic, glueing everything together.

### BoundedSequence

Sequence with a stack interface which drops elements pushed over a specified depth.

### CfgFile

Parse simple configuration file format:

```
# Comment
key = value
```

### CmdLineOptDefinitions

Jacky can be customized, debugged and profiled in many different ways. This module contains the definitions for the command line argument parser.

```
Usage: jacky [OPTION...]
  -o FILE    --oauth=FILE                   load OAuth details from FILE
                                            expected format / contents:
                                              oauth_consumer_key    = ...
                                              oauth_consumer_secret = ...
                                              oauth_token           = ...
                                              oauth_token_secret    = ...
                                            see https://dev.twitter.com/apps/new
             --firehose                     connect to firehose instead of user home timeline
  -f FOLDER  --log-folder=FOLDER            location of network data dump (default: ./log/)
  -l         --log-network                  dump all received API network data in log folder
  -r         --replay-log                   replay dumped API network data from log folder
  -i FOLDER  --http-imgcache-folder=FOLDER  image cache fldr (default: 'http_img_cache' in HOME)
             --conc-img-fetches=NUMBER      number of concurrent image fetches (default: 20)
  -m NUMBER  --img-mem-cache-size=NUMBER    number of images to keep in mem cache (default: 1024)
             --conn-keep-alive=NUMBER       # of conn. to single host to keep alive (default: 10)
             --conn-timeout=NUMBER          HTTP request timeout (in µs, default: 5000000)
  -s NUMBER  --stat-trace-interval=NUMBER   statistics tracing interval (in seconds, default: 10.0)
  -n NUMBER  --tweet-hist=NUMBER            number of tweets to keep around (default: 1024)
             --trace-file=FILE              output file for execution trace (default: ./trace.log)
  -t LEVEL   --trace-level=LEVEL            execution trace level (default: n)
                                              n = none
                                              e = errors only
                                              w = warnings and errors
                                              i = infos, warnings and errors
  -e         --trace-echo                   echo execution trace to stdout as well
             --trace-append                 append execution trace file instead of overwriting
             --trace-no-color               no ANSI colors for trace output (default: on)
             --verify-img-cache             debug: try to read & decode all cached images
             --ft2-test                     debug: output some FreeType 2 text to the terminal
             --force-autohint               force automatic hinting for FreeType 2 fonts
             --disable-kern                 disable kerning for FreeType 2 fonts
             --quad-rb-size=NUMBER          quad capacity font / UI OpenGL render buf. (default: 16384)
             --texture-pack-size=NUMBER     backing texture size for grid and atlas packing (default: 512)
             --dump-ft2-atlas-on-trace      dump the FreeType 2 texture atlas every trace interval
             --dump-texcache-grid-on-trace  dump the texture cache texture grid every trace interval
  -h         --help                         print usage information
```

### FontRendering

High-level module controlling OpenGL text layout and rendering. It relies on `FT2Interface` and `QuadRendering` for accessing OpenGL and FreeType 2 functionality.

- Support for multiple typefaces and Unicode glyphs
  ![Fonts](https://raw.github.com/blitzcode/jacky/master/img/fonts.png)
- Uses a `TextureAtlas` to pack glyph images into larger textures
  ![Glyph Atlas](https://raw.github.com/blitzcode/jacky/master/img/glyph_texture_atlas.png)
- Two caches (glyphs and kerning pairs) to minimize FT2 calls
- Basic text layout engine (`TLCenterHorz` | `TLCenterVert` | `TLAlignBottom` | `TLWordWrap` | `TLClipRect`)
  ![Text Layout](https://raw.github.com/blitzcode/jacky/master/img/text_layout.png)
- Supports simpler modes of operation without caches, texture atlas, simpler rendering through immediate mode quads / bitmaps

### FT2Interface, ft2_interface.c, ft2_interface.h

Custom, very simple and lightweight but fully-featured FreeType 2 bindings. Contains debug mode for dumping rendered glyphs / strings to the console.

![FT2 Console Test](https://raw.github.com/blitzcode/jacky/master/img/ft2_console_test.png)
![FT2 Console Test 2](https://raw.github.com/blitzcode/jacky/master/img/ft2_console_test_2.png)

### GLFWHelpers

The application uses the excellent GLFW-b package for windowing, input and OpenGL initialization.

### GLHelpers

Useful OpenGL stuff such as `setup2D`, `throwOnGLError` / `traceOnGLError`, `newTexture2D` and `saveTextureToPNG`.

### GLImmediate

Immediate mode OpenGL helper functions (for debugging, etc.).

### ImageCache

Caching system (disk & memory) for image fetches over HTTP and from disk. Handles retrieving images from Twitter, memory caching with an optional disk cache, retiring of least used elements at capacity, error handling / retrying, decompression (with `JuicyPixels`) / format conversion, request queues, avoiding double fetches, all concurrent, fast and robust. Internally uses `http-conduit`, our `LRUBoundedMap` and a pool of worker threads all synchronized using STM.

This was quite hard to get right, but it's very stable and fast now. All kinds of memory, connection and source image errors need to be handled and recovered from correctly, and the cache needs to purge the correct images when capacity is hit, all with potentially dozens of threads.

### LRUBoundedMap

Associative container (map / dictionary) which retires elements in least recently used (LRU) order when growing past a specified limit.

See the [LRUBoundedMap repository](https://github.com/blitzcode/lru-bounded-map) for details.

### Main

Initialization, OAuth code, startup of the Twitter status processing threads, command line parsing etc. Here we also handle streaming Twitter API data from a network dump (for debugging / reproducibility).

### ProcessStatus, TwitterJSON

Pick up Twitter status updates and related messages from a file or an HTTP connection and return the results as data structures from TwitterJSON. Also deal with issues such as REST API rate limits, disconnects / retries and authentication. Uses `conduit` and `attoparsec` / `aeson` internally.

### QuadRendering, QuadTypes, Shaders

Module for efficient rendering of 2D quad primitives, used for UI elements and texture mapped font rendering. Supports state sorting as well as back-to-front sorting for transparency. Uses OpenGL VAOs / VBOs / EBOs and GLSL shaders for high-performance rendering.

### QuadRenderingAdHoc

Inefficient / obsolete (just used for testing / development) immediate mode and ad-hoc drawing functions.

- OpenGL 1.0 style immediate mode drawing
- OpenGL 1.5 style VBO + FFP drawing with ad-hoc buffer creation
- OpenGL 3.0 style VBA / VBO + shader drawing, also using an index buffer, interleaved arrays and drawing triangles instead of quads. Buffers and shaders are created ad-hoc

### RectPacker

KDTree for finding a tight layout for a number of rectangles inside a larger bounding rectangle (bin packing). Useful for packing textures (fonts, lightmaps), UI elements, etc.

![Tiles](https://raw.github.com/blitzcode/jacky/master/img/tiles.png)

Supports online bin packing, uses a special optimization to keep KDTrees from degenerating:

```
For the right side, we have a special optimization. When
inserting many equally sized rectangles, our tree can
degenerate from O(log n) to O(n). To fix this, we merge
all those rectangles into a single one by moving the left
side of child split into the adjacent parent's left side
(just move the split position)

Example:

OOOOOOOOOOOOOOOOOOOOOO
O                    O
OOOOOOOOOOOOOOOOOOOOOO
O....O....O....O     O
O....O....O....O     O
OOOOOOOOOOOOOOOOOOOOOO

In this tree we inserted just three rectangles, but
already need four traversal steps for the next
insertion. With this optimization, all the bottom
rectangles get merged into a single space
```

### TextureAtlas

Pack multiple rectangular images into a set of OpenGL textures. Consider using the simpler / faster `TextureGrid` if the images are very similar in size.

![Texture Atlas](https://raw.github.com/blitzcode/jacky/master/img/texture_atlas.png)

### TextureGrid

Pack multiple rectangular images into a set of OpenGL textures. Unlike TextureAtlas, this module supports deletion of inserted images, but only packs them into a regular grid instead of using more sophisticated kD tree based bin packing. It's best used to pack smaller images of similar size. After insertion, we copy the image inside a slot + border sized container and fill the surrounding pixels by extruding the image, hopefully reducing bleeding artefacts (filtering / MIP-mapping). This is used for Twitter avatar images.

Example of a texture grid of 80x80 slots in a 512x512 texture with a one pixel border (notice the image border extrusion):

![Texture Grid](https://raw.github.com/blitzcode/jacky/master/img/texture_grid.png)

### TextureCache

OpenGL texture cache on top of the `ImageCache` module. Handles creation of textures and uploading into stand-alone texture or a `TextureGrid`.

### Timing

Provides `getTick` and `timeIt` helpers.

### Trace

Tracing system to allow all threads to print timestamped and categorized messages with verbosity levels and disk logging. ANSI colors (through `ansi-terminal`) are used for console output.

Example trace:

```
[~/jacky] ./dist/build/jacky/jacky -o ./src/tim.oauth -t i -e --firehose
INFO  | ThreadId 3   | 2014-05-25 18:12:26.756337
[FlagOAuthFile "./src/tim.oauth",FlagTraceLevel "i",FlagTraceEchoOn,FlagFirehose]

INFO  | ThreadId 3   | 2014-05-25 18:12:26.757653
OAuth {oauthServerName = "", oauthRequestUri = "", oauthAccessTokenUri = "", oauthAuthorizeUri = "", oauthSignatureMethod = HMACSHA1, oauthConsumerKey = "xxxxxxxxxxxxxxxxxxxxx", oauthConsumerSecret = "xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx", oauthCallback = Nothing, oauthRealm = Nothing, oauthVersion = OAuth10a}

INFO  | ThreadId 3   | 2014-05-25 18:12:26.758118
Credential {unCredential = [("oauth_token","xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx"),("oauth_token_secret","xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx")]}

INFO  | ThreadId 6   | 2014-05-25 18:12:26.806568
Twitter API request:
Request {
  host                 = "stream.twitter.com"
  port                 = 443
  secure               = True
  requestHeaders       = [("Authorization","OAuth oauth_signature=\"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\",oauth_signature_method=\"HMAC-SHA1\",oauth_consumer_key=\"xxxxxxxxxxxxxxxxxxxxx\",oauth_version=\"1.0\",oauth_timestamp=\"1401034346\",oauth_nonce=\"pglzygeopi\",oauth_token=\"xxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxxx\""),("Accept-Encoding","deflate, gzip"),("User-Agent","jacky/http-conduit")]
  path                 = "/1.1/statuses/sample.json"
  queryString          = ""
  method               = "GET"
  proxy                = Nothing
  rawBody              = False
  redirectCount        = 10
  responseTimeout      = Just (-3425)
}


INFO  | ThreadId 3   | 2014-05-25 18:12:26.926506
System - OS: darwin · Arch: i386 · CPUs: 2 · Compiler: ghc / Version {versionBranch = [7,6], versionTags = []} · FreeType Version: 2.4.7
OpenGL - Vendor: NVIDIA Corporation · Renderer: NVIDIA GeForce 9400M OpenGL Engine · Version: 2.1 NVIDIA-1.6.36 · GLSL: 1.20 · Num Extensions: 122 · GLFW: 3.0.3 Cocoa NSGL chdir menubar

INFO  | ThreadId 3   | 2014-05-25 18:12:26.928837 - Font: Futura, Medium · Hgt/Asc/Dsc: 32/25/-6px · 429Glyphs · NoKern
INFO  | ThreadId 3   | 2014-05-25 18:12:26.929268 - Font: Helvetica, Light · Hgt/Asc/Dsc: 48/37/-11px · 255Glyphs · Kern
INFO  | ThreadId 3   | 2014-05-25 18:12:26.929713 - Font: Lucida Grande, Regular · Hgt/Asc/Dsc: 38/31/-7px · 2826Glyphs · NoKern
INFO  | ThreadId 3   | 2014-05-25 18:12:26.930112 - Font: Verdana, Regular · Hgt/Asc/Dsc: 15/12/-3px · 950Glyphs · Kern
INFO  | ThreadId 3   | 2014-05-25 18:12:26.931067
Font: Arial Unicode MS, Regular · Hgt/Asc/Dsc: 21/17/-4px · 50377Glyphs · NoKern

INFO  | ThreadId 6   | 2014-05-25 18:12:28.35005 
Twitter API response from 'https://stream.twitter.com/1.1/statuses/sample.json'
Status: Status {statusCode = 200, statusMessage = "OK"}
Header: [("connection","close"),("content-Encoding","gzip"),("Content-Type","application/json"),("transfer-encoding","chunked"),("date","Sun, 25 May 2014 16:12:28 UTC"),("x-connection-hash","64ea57eb75ad958c1ae4103096760a32")]

INFO  | ThreadId 3   | 2014-05-25 18:12:36.933702
Font Rendering - CachedGlyphs: 671 · KernPairs: 1742 | AtlasTex: 1 x 512x512xAlpha8   Messages Total - SMTweet: 497 | SMDelete: 111 | Netw. Recv.: 1.634MB
Quad Rendering - Last drawRenderBuffer drawElementCalls: 19 · numQuad: 1813           Frametimes     - Mean: 112.3FPS/8.9ms | Worst: 31.9FPS/31.4ms | Best: 173.0FPS/5.8ms
GC             - maxUsed: 6.35MB · curUsed: 6.30MB · peakAlloc: 24MB | mutCPU: 6.52s · mutWall: 8.03s · gcCPU: 3.56s · gcWall: 2.15s · cpu: 10.10s · wall: 10.18s
Image Cache    - Netw. Recv. Total: 2.102MB · Mem 0.000MB | Req: 17/256 · Dir: 20/512 | Misses: 455 · DiskHits: 0 · MemHits: 8358 | Fetching: 20 · Fetched: 0 · Error: 0
Texture Cache  - Dir. Capacity: 455/1024 (100.0% slotrefs) · MemImg:   0MB · LargestImg: 0x0 | GridTex: 13 x 512x512xRGBA8 · 80x80 slots (free: 13)

ERROR | ThreadId 10  | 2014-05-25 18:12:40.380993
Image Cache Exception: DecodeException {deError = "Cannot load file\nJpeg Invalid marker used\nPNG Invalid PNG file, signature broken\nBitmap Invalid Bitmap magic identifier\nGIF demandInput: not enough bytes\nHDR demandInput: not enough bytes\nTiff Invalid endian tag value\n", deURI = "http://pbs.twimg.com/profile_images/1387640857/news_bigger.jpg", deCacheFn = "/Users/Tim/.http_img_cache/http%3A%2F%2Fpbs.twimg.com%2Fprofile_images%2F1387640857%2Fnews_bigger.jpg"}

WARN  | ThreadId 11  | 2014-05-25 18:12:42.384636
Now attempting retry no. 1 of failed URI fetch after >=2.0sec delay: http://pbs.twimg.com/profile_images/1387640857/news_bigger.jpg

ERROR | ThreadId 11  | 2014-05-25 18:12:42.435022
Image Cache Exception: DecodeException {deError = "Cannot load file\nJpeg Invalid marker used\nPNG Invalid PNG file, signature broken\nBitmap Invalid Bitmap magic identifier\nGIF demandInput: not enough bytes\nHDR demandInput: not enough bytes\nTiff Invalid endian tag value\n", deURI = "http://pbs.twimg.com/profile_images/1387640857/news_bigger.jpg", deCacheFn = "/Users/Tim/.http_img_cache/http%3A%2F%2Fpbs.twimg.com%2Fprofile_images%2F1387640857%2Fnews_bigger.jpg"}

INFO  | ThreadId 3   | 2014-05-25 18:12:46.94332 
Font Rendering - CachedGlyphs: 855 · KernPairs: 2624 | AtlasTex: 1 x 512x512xAlpha8   Messages Total - SMTweet: 1116 | SMDelete: 268 | Netw. Recv.: 3.666MB
Quad Rendering - Last drawRenderBuffer drawElementCalls: 34 · numQuad: 2171           Frametimes     - Mean: 82.0FPS/12.2ms | Worst: 33.2FPS/30.1ms | Best: 126.6FPS/7.9ms
GC             - maxUsed: 8.66MB · curUsed: 8.33MB · peakAlloc: 25MB | mutCPU: 13.76s · mutWall: 15.71s · gcCPU: 7.31s · gcWall: 4.48s · cpu: 21.09s · wall: 20.19s
Image Cache    - Netw. Recv. Total: 4.813MB · Mem 0.000MB | Req: 33/256 · Dir: 21/512 | Misses: 1051 · DiskHits: 0 · MemHits: 16764 | Fetching: 20 · Fetched: 0 · Error: 1
Texture Cache  - Dir. Capacity: 1024/1024 (100.0% slotrefs) · MemImg:   0MB · LargestImg: 0x0 | GridTex: 29 x 512x512xRGBA8 · 80x80 slots (free: 20)

WARN  | ThreadId 17  | 2014-05-25 18:12:52.442614
Now attempting retry no. 2 of failed URI fetch after >=10.0sec delay: http://pbs.twimg.com/profile_images/1387640857/news_bigger.jpg

ERROR | ThreadId 17  | 2014-05-25 18:12:52.503775
Image Cache Exception: DecodeException {deError = "Cannot load file\nJpeg Invalid marker used\nPNG Invalid PNG file, signature broken\nBitmap Invalid Bitmap magic identifier\nGIF demandInput: not enough bytes\nHDR demandInput: not enough bytes\nTiff Invalid endian tag value\n", deURI = "http://pbs.twimg.com/profile_images/1387640857/news_bigger.jpg", deCacheFn = "/Users/Tim/.http_img_cache/http%3A%2F%2Fpbs.twimg.com%2Fprofile_images%2F1387640857%2Fnews_bigger.jpg"}

INFO  | ThreadId 3   | 2014-05-25 18:12:56.956729
Font Rendering - CachedGlyphs: 1014 · KernPairs: 3458 | AtlasTex: 1 x 512x512xAlpha8  Messages Total - SMTweet: 1719 | SMDelete: 421 | Netw. Recv.: 5.601MB
Quad Rendering - Last drawRenderBuffer drawElementCalls: 31 · numQuad: 2252           Frametimes     - Mean: 76.2FPS/13.1ms | Worst: 22.0FPS/45.5ms | Best: 104.8FPS/9.5ms
GC             - maxUsed: 16.59MB · curUsed: 16.51MB · peakAlloc: 49MB | mutCPU: 20.81s · mutWall: 23.35s · gcCPU: 11.24s · gcWall: 6.85s · cpu: 32.07s · wall: 30.20s
Image Cache    - Netw. Recv. Total: 7.670MB · Mem 0.020MB | Req: 24/256 · Dir: 22/512 | Misses: 1649 · DiskHits: 0 · MemHits: 25163 | Fetching: 20 · Fetched: 1 · Error: 1
Texture Cache  - Dir. Capacity: 1024/1024 (100.0% slotrefs) · MemImg:   0MB · LargestImg: 0x0 | GridTex: 29 x 512x512xRGBA8 · 80x80 slots (free: 20)

INFO  | ThreadId 3   | 2014-05-25 18:12:58.311921 - Shutting down FreeType
INFO  | ThreadId 3   | 2014-05-25 18:12:58.337431 - Shutting down texture cache
INFO  | ThreadId 3   | 2014-05-25 18:12:58.416046 - Shutting down image fetch threads
INFO  | ThreadId 6   | 2014-05-25 18:12:58.417122
Recv. ThreadKilled while processing statuses from 'https://stream.twitter.com/1.1/statuses/sample.json', exiting
thread killed

INFO  | ThreadId 3   | 2014-05-25 18:12:58.417293 - Clean Exit
INFO  | ThreadId 3   | 2014-05-25 18:12:58.417393 - Shutting down trace system
```

At a user specified interval a statistics trace is echoed, allowing debugging of space leaks, cache efficiency, OpenGL performance, font rendering, GC, UI stuttering, Twitter REST API and network communication:

![Statistics Trace](https://raw.github.com/blitzcode/jacky/master/img/stat_trace.png)

This proved to be invaluable for tracking down all kinds of issues.

### UI

Drawing, layout and event handling for the user interface.

# Legal

This program is published under the [MIT License](http://en.wikipedia.org/wiki/MIT_License).

Portions of this software are copyright © 2013 The FreeType Project (www.freetype.org).  All rights reserved.

# Author

Developed by Tim C. Schroeder, visit my [website](http://www.blitzcode.net) to learn more.

