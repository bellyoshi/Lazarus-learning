// ******************************************************************
// Compiler: FPC 3.1.1, CodeTyphon 540
// Author: Dinko Miljak,
// Original Template: MuPDF lib header port to FPC (c) 2013 by Blestan Tabakov
// Date: 2016-01-21
// This unit is obsolete and TPDFDoc does not use it any more
// Version 1.6 and version 1.8 calls are different becasue almost
// every function have fctx as first param and some functions 
// replaced fdoc  with fctx. Some functions are rename 
// free_ functions are drop_ functions in ver 1.8
// If you need to use version 1.6 then pay good attention how every
// function is called in TPDFDoc unit (access violation are possible 
// if functions are not called correctly)
// ******************************************************************
unit libmupdf16;

{$mode objfpc}{$H+}

{$IFDEF FPC}
  {$PackEnum 4}    // use 4-byte enums
  {$PACKRECORDS C} // C/C++-compatible record packing
{$ELSE}
  {$MINENUMSIZE 4} // use 4-byte enums
{$ENDIF}

// {$A1}    // use 4-byte enums

interface

uses
  Classes, SysUtils,ctypes;


const
{$IF Defined(MSWINDOWS)}
  {$IFDEF WIN32}
  muLibName = 'libmupdf16-32.dll';
  {$ELSE}
  muLibName = 'libmupdf16-64.dll';
  {$ENDIF}
{$ELSEIF Defined(DARWIN)}
  muLibName = 'libmupdf.dylib';
  {$LINKLIB mylib}
{$ELSEIF Defined(UNIX)}
  muLibName = 'libmupdf.so';
  // muLibName = 'fitz';
  {uncomment the following lines to link statically instead of to a shared library}
  //{$LINKLIB /path-to/mupdf-1.2-source/build/debug/libfitz.a}
  //{$LINKLIB /path-to/mupdf-1.2-source/build/debug/libfreetype.a}
  //{$LINKLIB /path-to/mupdf-1.2-source/build/debug/libopenjpeg.a}
  //{$LINKLIB /path-to/mupdf-1.2-source/build/debug/libjbig2dec.a}
  //{$LINKLIB /path-to/mupdf-1.2-source/build/debug/libjpeg.a}
{$IFEND}

FZ_VERSION: String = '1.6';


type

    pfz_bbox = ^ fz_bbox;
    fz_bbox = record
               x0,y0,x1,y1: cint
              end;

    pfz_rectangle=^fz_rectangle;
    fz_rectangle= record
                   x0,y0,x1,y1: cfloat
                  end;

    fz_matrix = record
                 a,b,c,d,e,f: cfloat
                end;

    fz_irect = record
	    x0, y0: cint;
	    x1, y1: cint;
    end;

    fz_document_handler = record
      recognize: Pointer; // fz_document_recognize_fn
      open: Pointer; // fz_document_open_fn *open;
      open_with_stream: Pointer; // fz_document_open_with_stream_fn;
    end;


// pointer types to various internal structures. Should be considered as OPAQUE for now :)


 fz_context = pointer;
 fz_document = pointer;
 fz_page= pointer;
 fz_device = pointer;

{
    Pixmaps represent a set of pixels for a 2 dimensional region of a
    plane. Each pixel has n components per pixel, the last of which is
    always alpha. The data is in premultiplied alpha when rendering, but
    non-premultiplied for colorspace conversions and rescaling.
}
 fz_pixmap = pointer;

 {
    An fz_colorspace object represents an abstract colorspace. While
    this should be treated as a black box by callers of the library at
    this stage, know that it encapsulates knowledge of how to convert
    colors to and from the colorspace, any lookup tables generated, the
    number of components in the colorspace etc.
}

 fz_colorspace = pointer;

 {
      Fitz COOKIE - simple communication channel between app/library.
      Provide two-way communication between application and library.
      Intended for multi-threaded applications where one thread is
      rendering pages and another thread wants read progress
      feedback or abort a job that takes a long time to finish. The
      communication is unsynchronized without locking.

      abort: The appliation should set this field to 0 before
      calling fz_run_page to render a page. At any point when the
      page is being rendered the application my set this field to 1
      which will cause the rendering to finish soon. This field is
      checked periodically when the page is rendered, but exactly
      when is not known, therefore there is no upper bound on
      exactly when the the rendering will abort. If the application
      did not provide a set of locks to fz_new_context, it must also
      await the completion of fz_run_page before issuing another
      call to fz_run_page. Note that once the application has set
      this field to 1 after it called fz_run_page it may not change
      the value again.

      progress: Communicates rendering progress back to the
      application and is read only. Increments as a page is being
      rendered. The value starts out at 0 and is limited to less
      than or equal to progress_max, unless progress_max is -1.

      progress_max: Communicates the known upper bound of rendering
      back to the application and is read only. The maximum value
      that the progress field may take. If there is no known upper
      bound on how long the rendering may take this value is -1 and
      progress is not limited. Note that the value of progress_max
      may change from -1 to a positive value once an upper bound is
      known, so take this into consideration when comparing the
      value of progress to that of progress_max.

      errors: count of errors during current rendering.
 }


type

      fz_cookie = ^fz_cookie_rec;
      fz_cookie_rec= record
               abort,
               progress,
               progress_max, // -1 for unknown
               errors: cint
           end;


VAR
  pdf_document_handler: fz_document_handler; EXTERNAL muLibName NAME 'pdf_document_handler';
{ Another way.  It does compiles but aborts as well. }
//  another_c_string: STRING; EXTERNAL 'the_lib_file.so' NAME 'the_c_var_name';

const

// Fitz CONTEXT

{   Specifies the maximum size in bytes of the resource store in
    fz_context. Given as argument to fz_new_context.

    FZ_STORE_UNLIMITED: Let resource store grow unbounded.

    FZ_STORE_DEFAULT: A reasonable upper bound on the size, for
    devices that are not memory constrained.
}
    FZ_STORE_UNLIMITED = 0;
    FZ_STORE_DEFAULT = 256 << 20;

 {
      fz_new_context: Allocate context containing global state.

      The global state contains an exception stack, resource store,
      etc. Most functions in MuPDF take a context argument to be
      able to reference the global state. See fz_free_context for
      freeing an allocated context.

      alloc: Supply a custom memory allocator through a set of
      function pointers. Set to NULL for the standard library
      allocator. The context will keep the allocator pointer, so the
      data it points to must not be modified or freed during the lifetime
      of the context.

      locks: Supply a set of locks and functions to lock/unlock
      them, intended for multi-threaded applications. Set to NULL
      when using MuPDF in a cfloat-threaded applications. The
      context will keep the locks pointer, so the data it points to
      must not be modified or freed during the lifetime of the
      context.

      max_store: Maximum size in bytes of the resource store, before
      it will start evicting cached resources such as fonts and
      images. FZ_STORE_UNLIMITED can be used if a hard limit is not
      desired. Use FZ_STORE_DEFAULT to get a reasonable size.

      Does not throw exceptions, but may return NULL.
}

      function fz_new_context_imp(alloc:Pointer; locks: Pointer; max_store: cuint; const version: PChar): fz_context;cdecl;external muLibName name 'fz_new_context_imp';
      function fz_new_context(alloc:Pointer; locks: Pointer; max_store: cuint): fz_context;
      procedure fz_new_document_handler_context(ctx: fz_context);cdecl;external muLibName name 'fz_new_document_handler_context';
      procedure fz_drop_document_handler_context(ctx: fz_context);cdecl;external muLibName name 'fz_drop_document_handler_context';
      // void fz_register_document_handler(fz_context *ctx, const fz_document_handler *handler)
      procedure fz_register_document_handler(ctx: fz_context; var fz_document_handler);cdecl;external muLibName name 'fz_register_document_handler';
      procedure fz_register_document_handlers(ctx: fz_context);

//      function fz_dinko(pero: cint; pero2: int32; var fz_rect: fz_rectangle): fz_rectangle;cdecl;external muLibName name 'fz_dinko';
//      procedure fz_dinko(pero: cint; var pero2: cint; var fz_rect: fz_rectangle);cdecl;external muLibName name 'fz_dinko';


      {
      fz_free_context: Free a context and its global state.

      The context and all of its global state is freed, and any
      buffered warnings are flushed (see fz_flush_warnings). If NULL
      is passed in nothing will happen.

      Does not throw exceptions.
}

     procedure fz_free_context_ex(ctx: fz_context);cdecl;external muLibName name 'fz_free_context';
     procedure fz_free_context(ctx: fz_context);

     procedure fz_rotate(var m: fz_matrix; theta: cfloat);cdecl;external muLibName name 'fz_rotate';
     procedure fz_pre_scale(var mat: fz_matrix; sx: cfloat; sy: cfloat);cdecl;external muLibName name 'fz_pre_scale';


// Fitz COLORSPACE

{    fz_find_device_colorspace: Find a standard colorspace based upon it's name. }

//     function fz_find_device_colorspace(ctx:fz_context; name: pchar):fz_colorspace;cdecl;external muLibName name 'fz_find_device_colorspace';
     function fz_find_device_colorspace(ctx:fz_context; name: pchar):fz_colorspace;cdecl;external muLibName name 'fz_lookup_device_colorspace';
{
{    fz_device_gray: Abstract colorspace representing device specific gray.}

     function  fz_device_gray:fz_colorspace;cdecl;external muLibName name 'fz_device_gray';

{     fz_device_rgb: Abstract colorspace representing device specific rgb. }

     function  fz_device_rgb:fz_colorspace;cdecl;external muLibName name 'fz_device_rgb';


{     fz_device_rgb: Abstract colorspace representing device specific bgr. }

     function  fz_device_bgr:fz_colorspace;cdecl;external muLibName name 'fz_device_bgr';


{     fz_device_rgb: Abstract colorspace representing device specific CMYK. }

     function  fz_device_cmyk:fz_colorspace;cdecl;external muLibName name 'fz_device_cmyk';

}

//   Fitz Pixmap

{     fz_pixmap_bbox: Return a bounding box for a pixmap.

      Returns an exact bounding box for the supplied pixmap.
}

     function fz_pixmap_bbox(ctx:fz_context; pix:fz_pixmap):fz_bbox;cdecl;external muLibName name 'fz_pixmap_bbox';

{     fz_pixmap_width: Return the width of the pixmap in pixels. }

     function fz_pixmap_width(ctx:fz_context; pix: fz_pixmap):cint;cdecl;external muLibName name 'fz_pixmap_width';

{     fz_pixmap_height: Return the height of the pixmap in pixels.}

      function fz_pixmap_height(ctx:fz_context; pix: fz_pixmap):cint;cdecl;external muLibName name 'fz_pixmap_height';

{     fz_new_pixmap: Create a new pixmap, with it's origin at (0,0)

         cs: The colorspace to use for the pixmap, or NULL for an alpha plane/mask.

         w: The width of the pixmap (in pixels)

         h: The height of the pixmap (in pixels)

         Returns a pointer to the new pixmap. Throws exception on failure to allocate.
}

     function fz_new_pixmap(ctx:fz_context; cs:fz_colorspace; w,h: cint):fz_pixmap;cdecl;external muLibName name 'fz_new_pixmap';

{
     fz_new_pixmap_with_bbox: Create a pixmap of a given size,
     location and pixel format.

     The bounding box specifies the size of the created pixmap and
     where it will be located. The colorspace determines the number
     of components per pixel. Alpha is always present. Pixmaps are
     reference counted, so drop references using fz_drop_pixmap.

     colorspace: Colorspace format used for the created pixmap. The
     pixmap will keep a reference to the colorspace.

     bbox: Bounding box specifying location/size of created pixmap.

     Returns a pointer to the new pixmap. Throws exception on failure to allocate.
}

     function fz_new_pixmap_with_bbox(ctx: fz_context; cs: fz_colorspace; bbox: fz_bbox):fz_pixmap;cdecl;external muLibName name 'fz_new_pixmap_with_bbox';

{
         fz_new_pixmap_with_data: Create a new pixmap, with it's origin at
         (0,0) using the supplied data block.

         cs: The colorspace to use for the pixmap, or NULL for an alpha
         plane/mask.

         w: The width of the pixmap (in pixels)

         h: The height of the pixmap (in pixels)

         samples: The data block to keep the samples in.

         Returns a pointer to the new pixmap. Throws exception on failure to
         allocate.
}

         function fz_new_pixmap_with_data(ctx:fz_context; cs:fz_colorspace; w,h: cint; samples: pointer):fz_pixmap;cdecl;external muLibName name 'fz_new_pixmap_with_data';

{
         fz_new_pixmap_with_bbox_and_data: Create a pixmap of a given size,
         location and pixel format, using the supplied data block.

         The bounding box specifies the size of the created pixmap and
         where it will be located. The colorspace determines the number
         of components per pixel. Alpha is always present. Pixmaps are
         reference counted, so drop references using fz_drop_pixmap.

         colorspace: Colorspace format used for the created pixmap. The
         pixmap will keep a reference to the colorspace.

         bbox: Bounding box specifying location/size of created pixmap.

         samples: The data block to keep the samples in.

         Returns a pointer to the new pixmap. Throws exception on failure to
         allocate.
}

        // fz_pixmap *fz_new_pixmap_with_bbox_and_data(fz_context *ctx, fz_colorspace *colorspace, const fz_irect *r, unsigned char *samples)
        function fz_new_pixmap_with_bbox_and_data(ctx: fz_context; cs: fz_colorspace; var r: fz_irect; samples: pointer):fz_pixmap;cdecl;external muLibName name 'fz_new_pixmap_with_bbox_and_data';


{
       fz_keep_pixmap: Take a reference to a pixmap.

       pix: The pixmap to increment the reference for.

       Returns pix. Does not throw exceptions.
}

     function  fz_keep_pixmap(ctx: fz_context; pix: fz_pixmap):fz_pixmap;cdecl;external muLibName name 'fz_keep_pixmap';

{
       fz_drop_pixmap: Drop a reference and free a pixmap.

       Decrement the reference count for the pixmap. When no
       references remain the pixmap will be freed.

       Does not throw exceptions.
}

     procedure fz_drop_pixmap(ctx: fz_context; pix: fz_pixmap);cdecl;external muLibName name 'fz_drop_pixmap';


{
       fz_pixmap_colorspace: Return the colorspace of a pixmap

        Returns colorspace. Does not throw exceptions.
}

     function  fz_pixmap_colorspace(ctx: fz_context; pix:fz_pixmap):fz_colorspace;cdecl;external muLibName name 'fz_pixmap_colorspace';

{
       fz_pixmap_components: Return the number of components in a pixmap.

       Returns the number of components. Does not throw exceptions.
}
     function fz_pixmap_components(ctx:fz_context; pix:fz_pixmap):cint;cdecl;external muLibName name 'fz_pixmap_components';


{
      fz_pixmap_samples: Returns a pointer to the pixel data of a pixmap.

      Returns the pointer. Does not throw exceptions.
}

     function fz_pixmap_samples(ctx: fz_context; pix: fz_pixmap): pointer;cdecl;external muLibName name 'fz_pixmap_samples';

{
      fz_clear_pixmap_with_value: Clears a pixmap with the given value.

      pix: The pixmap to clear.

      value: Values in the range 0 to 255 are valid. Each component
      sample for each pixel in the pixmap will be set to this value,
      while alpha will always be set to 255 (non-transparent).
      Does not throw exceptions.
}
     procedure fz_clear_pixmap_with_value(ctx:fz_context; pix: fz_pixmap; value: cint);cdecl;external muLibName name 'fz_clear_pixmap_with_value';

{
         fz_clear_pixmap: Sets all components (including alpha) of
         all pixels in a pixmap to 0.

         pix: The pixmap to clear.

         Does not throw exceptions.
}
     procedure fz_clear_pixmap(ctx:fz_context; pix:fz_pixmap);cdecl;external muLibName name 'fz_clear_pixmap';


// Fitz DEVICE

     {
         The different format handlers (pdf, xps etc) interpret pages to a
         device. These devices can then process the stream of calls they
         recieve in various ways:
             The trace device outputs debugging information for the calls.
             The draw device will render them.
             The list device stores them in a list to play back later.
             The text device performs text extraction and searching.
             The bbox device calculates the bounding box for the page.
         Other devices can (and will) be written in future.
     }


     {    fz_free_device: Free a devices of any type and its resources.}

          procedure fz_free_device(dev: fz_device);cdecl;external muLibName name 'fz_free_device';


     {    fz_new_trace_device: Create a device to print a debug trace of all device calls. }

          function  fz_new_trace_device(ctx:fz_context):fz_device;cdecl;external muLibName name 'fz_new_trace_device';

     {   fz_new_bbox_device: Create a device to compute the bounding box of all marks on a page.

         The returned bounding box will be the union of all bounding
         boxes of all objects on a page.
     }
         function  fz_new_bbox_device(ctx: fz_context; bboxp: pfz_bbox):fz_device;cdecl;external muLibName name 'fz_new_bbox_device';

     {
         fz_new_draw_device: Create a device to draw on a pixmap.

         dest: Target pixmap for the draw device. See fz_new_pixmap*
         for how to obtain a pixmap. The pixmap is not cleared by the
         draw device, see fz_clear_pixmap* for how to clear it prior to
         calling fz_new_draw_device. Free the device by calling
         fz_free_device.
     }

      function fz_new_draw_device(ctx: fz_context; dest: fz_pixmap):fz_device;cdecl;external muLibName name 'fz_new_draw_device';

     {
         fz_new_draw_device_with_bbox: Create a device to draw on a pixmap.

         dest: Target pixmap for the draw device. See fz_new_pixmap*
         for how to obtain a pixmap. The pixmap is not cleared by the
         draw device, see fz_clear_pixmap* for how to clear it prior to
         calling fz_new_draw_device. Free the device by calling
         fz_free_device.

         clip: Bounding box to restrict any marking operations of the
         draw device.
     }

     function fz_new_draw_device_with_bbox(ctx: fz_context; dest: fz_pixmap; clip: fz_bbox):fz_device;cdecl;external muLibName name 'fz_new_draw_device_with_bbox';

// Fitz DOCUMENT

{
      fz_open_document: Open a PDF, XPS or CBZ document.

      Open a document file and read its basic structure so pages and
      objects can be located. MuPDF will try to repair broken
      documents (without actually changing the file contents).

      The returned fz_document is used when calling most other
      document related functions. Note that it wraps the context, so
      those functions implicitly can access the global state in context.

      filename: a path to a file as it would be given to open(2).
}

      function fz_open_document(ctx: fz_context; const filename: PChar): fz_document;cdecl;external muLibName name 'fz_open_document';

{
      fz_close_document: Close and free an open document.

      The resource store in the context associated with fz_document
      is emptied, and any allocations for the document are freed.

      Does not throw exceptions.
}

      procedure fz_close_document(doc: fz_document);cdecl;external muLibName name 'fz_close_document';


{
      fz_needs_password: Check if a document is encrypted with a
      non-blank password.
      Does not throw exceptions.
}

      function fz_needs_password(doc: fz_document):cint;cdecl;external muLibName name 'fz_needs_password';

{
      fz_authenticate_password: Test if the given password can
      decrypt the document.
      password: The password string to be checked. Some document
      specifications do not specify any particular text encoding, so
      neither do we.

      Does not throw exceptions.
}
      function fz_authenticate_password(doc: fz_document; password: PChar):cint;cdecl;external muLibName name 'fz_authenticate_password';

{
    fz_count_pages: Return the number of pages in document

    May return 0 for documents with no pages.
}


      function fz_count_pages(doc: fz_document):cint;cdecl;external muLibName name 'fz_count_pages';


// Fitz Page


{
     fz_load_page: Load a page.

     After fz_load_page is it possible to retrieve the size of the
     page using fz_bound_page, or to render the page using
     fz_run_page_*. Free the page by calling fz_free_page.

     number: page number, 0 is the first page of the document.
}

     function fz_load_page(doc: fz_document; number: cint):fz_page;cdecl;external muLibName name 'fz_load_page';

{
    fz_free_page: Free a loaded page.

    Does not throw exceptions.
}
    procedure fz_free_page(doc: fz_document; page: fz_page);cdecl;external muLibName name 'fz_free_page';

{   fz_bound_page: Determine the size of a page at 72 dpi. }

    procedure fz_bound_page(doc: fz_document; page: fz_page; var r: fz_rectangle);cdecl;external muLibName name 'fz_bound_page';


{
    fz_run_page: Run a page through a device.

    page: Page obtained from fz_load_page.

    dev: Device obtained from fz_new_*_device.

    transform: Transform to apply to page. May include for example
    scaling and rotation, see fz_scale, fz_rotate and fz_concat.
    Set to fz_identity if no transformation is desired.

    cookie: Communication mechanism between caller and library
    rendering the page. Intended for multi-threaded applications,
    while cfloat-threaded applications set cookie to NULL. The
    caller may abort an ongoing rendering of a page. Cookie also
    communicates progress information back to the caller. The
    fields inside cookie are continually updated while the page is
    rendering.
}

    /// void fz_run_page(fz_document *doc, fz_page *page, fz_device *dev, const fz_matrix *transform, fz_cookie *cookie)
    procedure fz_run_page(doc:fz_document; page: fz_page; dev: fz_device; var transform: fz_matrix; cookie: fz_cookie);cdecl;external muLibName name 'fz_run_page';



 {
	fz_transform_rect: Apply a transform to a rectangle.

	After the four corner points of the axis-aligned rectangle
	have been transformed it may not longer be axis-aligned. So a
	new axis-aligned rectangle is created covering at least the
	area of the transformed rectangle.

	transform: Transformation matrix to apply. See fz_concat,
	fz_scale and fz_rotate for how to create a matrix.

	rect: Rectangle to be transformed. The two special cases
	fz_empty_rect and fz_infinite_rect, may be used but are
	returned unchanged as expected.

	Does not throw exceptions.
}

procedure fz_transform_rect(var rect :  fz_rectangle; var transform: fz_matrix);cdecl;external muLibName name 'fz_transform_rect';

const
  Init_fz_bbox: fz_bbox = ();
  Init_fz_rectangle: fz_rectangle = ();
  Init_fz_matrix: fz_matrix = ();
  Init_fz_irect: fz_irect = ();


implementation

function fz_new_context(alloc: Pointer; locks: Pointer; max_store: cuint
  ): fz_context;
var MyContext: fz_context;
begin
  MyContext := nil;
  MyContext := fz_new_context_imp(alloc, locks, max_store, PChar(FZ_VERSION));
  if MyContext <> nil then begin
    fz_new_document_handler_context(MyContext);
    fz_register_document_handler(MyContext, pdf_document_handler);
  end;
  Result := MyContext;
end;

procedure fz_register_document_handlers(ctx: fz_context);
begin
	fz_register_document_handler(ctx, &pdf_document_handler);
//	fz_register_document_handler(ctx, &xps_document_handler);
//	fz_register_document_handler(ctx, &cbz_document_handler);
//	fz_register_document_handler(ctx, &img_document_handler);
//	fz_register_document_handler(ctx, &tiff_document_handler);
end;

procedure fz_free_context(ctx: fz_context);
begin
  if ctx <> nil then begin
    fz_drop_document_handler_context(ctx);
    fz_free_context_ex(ctx);
  end;
end;



end.

