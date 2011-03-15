(define-foreign-library libfreetype
  (t (:default "libfreetype")))
(use-foreign-library libfreetype)

;;;SWIG wrapper code starts here

(cl:defmacro defanonenum (&body enums)
   "Converts anonymous enums to defconstants."
  `(cl:progn ,@(cl:loop for value in enums
                        for index = 0 then (cl:1+ index)
                        when (cl:listp value) do (cl:setf index (cl:second value)
                                                          value (cl:first value))
                        collect `(cl:defconstant ,value ,index))))

(cl:eval-when (:compile-toplevel :load-toplevel)
  (cl:unless (cl:fboundp 'swig-lispify)
    (cl:defun swig-lispify (name flag cl:&optional (package cl:*package*))
      (cl:labels ((helper (lst last rest cl:&aux (c (cl:car lst)))
                    (cl:cond
                      ((cl:null lst)
                       rest)
                      ((cl:upper-case-p c)
                       (helper (cl:cdr lst) 'upper
                               (cl:case last
                                 ((lower digit) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:lower-case-p c)
                       (helper (cl:cdr lst) 'lower (cl:cons (cl:char-upcase c) rest)))
                      ((cl:digit-char-p c)
                       (helper (cl:cdr lst) 'digit 
                               (cl:case last
                                 ((upper lower) (cl:list* c #\- rest))
                                 (cl:t (cl:cons c rest)))))
                      ((cl:char-equal c #\_)
                       (helper (cl:cdr lst) '_ (cl:cons #\- rest)))
                      (cl:t
                       (cl:error "Invalid character: ~A" c)))))
        (cl:let ((fix (cl:case flag
                        ((constant enumvalue) "+")
                        (variable "*")
                        (cl:t ""))))
          (cl:intern
           (cl:concatenate
            'cl:string
            fix
            (cl:nreverse (helper (cl:concatenate 'cl:list name) cl:nil cl:nil))
            fix)
           package))))))

;;;SWIG wrapper code ends here


(cl:defconstant FT_USE_AUTOCONF_SIZEOF_TYPES 1)

(cl:defconstant FT_RENDER_POOL_SIZE 16384)

(cl:defconstant FT_MAX_MODULES 32)

(cl:defconstant T1_MAX_DICT_DEPTH 5)

(cl:defconstant T1_MAX_SUBRS_CALLS 16)

(cl:defconstant T1_MAX_CHARSTRINGS_OPERANDS 256)

(cl:defconstant HAVE_UNISTD_H 1)

(cl:defconstant HAVE_FCNTL_H 1)

(cl:defconstant HAVE_STDINT_H 1)

(cl:defconstant SIZEOF_INT 4)

(cl:defconstant SIZEOF_LONG 4)

(cl:defconstant FT_SIZEOF_INT 4)

(cl:defconstant FT_SIZEOF_LONG 4)

(cl:defconstant FT_ALIGNMENT 8)

(cl:defconstant FT_ERR_BASE 0)

(defanonenum 
	FT_Err_Max)

(cffi:defcstruct FT_MemoryRec_
	(user :pointer)
	(alloc :pointer)
	(free :pointer)
	(realloc :pointer))

(cffi:defcunion FT_StreamDesc
	(value :long)
	(pointer :pointer))

(cffi:defcstruct FT_StreamRec
	(base :pointer)
	(size :unsigned-long)
	(pos :unsigned-long)
	(descriptor FT_StreamDesc)
	(pathname FT_StreamDesc)
	(read :pointer)
	(close :pointer)
	(memory :pointer)
	(cursor :pointer)
	(limit :pointer))

(cffi:defcstruct FT_Vector
	(x :long)
	(y :long))

(cffi:defcstruct FT_BBox
	(xMin :long)
	(yMin :long)
	(xMax :long)
	(yMax :long))

(cffi:defcenum FT_Pixel_Mode
	(:FT_PIXEL_MODE_NONE 0)
	:FT_PIXEL_MODE_MONO
	:FT_PIXEL_MODE_GRAY
	:FT_PIXEL_MODE_GRAY2
	:FT_PIXEL_MODE_GRAY4
	:FT_PIXEL_MODE_LCD
	:FT_PIXEL_MODE_LCD_V
	:FT_PIXEL_MODE_MAX)

(cffi:defcstruct FT_Bitmap
	(rows :int)
	(width :int)
	(pitch :int)
	(buffer :pointer)
	(num_grays :short)
	(pixel_mode :char)
	(palette_mode :char)
	(palette :pointer))

(cffi:defcstruct FT_Outline
	(n_contours :short)
	(n_points :short)
	(points :pointer)
	(tags :string)
	(contours :pointer)
	(flags :int))

;; Added by hand
(cl:defconstant FT_ENCODING_UNICODE (logior (ash (char-code #\u) 24)
					    (ash (char-code #\n) 16)
					    (ash (char-code #\i) 8)
					    (ash (char-code #\c) 0)))

(cl:defconstant FT_OUTLINE_NONE #x0)

(cl:defconstant FT_OUTLINE_OWNER #x1)

(cl:defconstant FT_OUTLINE_EVEN_ODD_FILL #x2)

(cl:defconstant FT_OUTLINE_REVERSE_FILL #x4)

(cl:defconstant FT_OUTLINE_IGNORE_DROPOUTS #x8)

(cl:defconstant FT_OUTLINE_SMART_DROPOUTS #x10)

(cl:defconstant FT_OUTLINE_INCLUDE_STUBS #x20)

(cl:defconstant FT_OUTLINE_HIGH_PRECISION #x100)

(cl:defconstant FT_OUTLINE_SINGLE_PASS #x200)

(cl:defconstant ft_outline_none #x0)

(cl:defconstant ft_outline_owner #x1)

(cl:defconstant ft_outline_even_odd_fill #x2)

(cl:defconstant ft_outline_reverse_fill #x4)

(cl:defconstant ft_outline_ignore_dropouts #x8)

(cl:defconstant ft_outline_high_precision #x100)

(cl:defconstant ft_outline_single_pass #x200)

(cl:defconstant FT_CURVE_TAG_ON 1)

(cl:defconstant FT_CURVE_TAG_CONIC 0)

(cl:defconstant FT_CURVE_TAG_CUBIC 2)

(cl:defconstant FT_CURVE_TAG_TOUCH_X 8)

(cl:defconstant FT_CURVE_TAG_TOUCH_Y 16)

(cl:defconstant FT_CURVE_TAG_TOUCH_BOTH (cl:logior 8 16))

(cl:defconstant FT_Curve_Tag_On 1)

(cl:defconstant FT_Curve_Tag_Conic 0)

(cl:defconstant FT_Curve_Tag_Cubic 2)

(cl:defconstant FT_Curve_Tag_Touch_X 8)

(cl:defconstant FT_Curve_Tag_Touch_Y 16)

(cffi:defcstruct FT_Outline_Funcs
	(move_to :pointer)
	(line_to :pointer)
	(conic_to :pointer)
	(cubic_to :pointer)
	(shift :int)
	(delta :long))

(cffi:defctype FT_Glyph_Format :int)

#|
(cffi:defcenum FT_Glyph_Format
	(:FT_GLYPH_FORMAT_NONE (cl:logior (nsigned ong) 0 << 24 (nsigned ong) 0 << 16 (nsigned ong) 0 << 8 (nsigned ong) 0))
	(:FT_GLYPH_FORMAT_COMPOSITE (cl:logior (nsigned ong) c << 24 (nsigned ong) o << 16 (nsigned ong) m << 8 (nsigned ong) p))
	(:FT_GLYPH_FORMAT_BITMAP (cl:logior (nsigned ong) b << 24 (nsigned ong) i << 16 (nsigned ong) t << 8 (nsigned ong) s))
	(:FT_GLYPH_FORMAT_OUTLINE (cl:logior (nsigned ong) o << 24 (nsigned ong)  << 16 (nsigned ong) t << 8 (nsigned ong) ))
	(:FT_GLYPH_FORMAT_PLOTTER (cl:logior (nsigned ong) p << 24 (nsigned ong)  << 16 (nsigned ong) o << 8 (nsigned ong) t)))
|#

(cffi:defcstruct FT_Span
	(x :short)
	(len :unsigned-short)
	(coverage :unsigned-char))

(cl:defconstant FT_RASTER_FLAG_DEFAULT #x0)

(cl:defconstant FT_RASTER_FLAG_AA #x1)

(cl:defconstant FT_RASTER_FLAG_DIRECT #x2)

(cl:defconstant FT_RASTER_FLAG_CLIP #x4)

(cl:defconstant ft_raster_flag_default #x0)

(cl:defconstant ft_raster_flag_aa #x1)

(cl:defconstant ft_raster_flag_direct #x2)

(cl:defconstant ft_raster_flag_clip #x4)

(cffi:defcstruct FT_Raster_Params
	(target :pointer)
	(source :pointer)
	(flags :int)
	(gray_spans :pointer)
	(black_spans :pointer)
	(bit_test :pointer)
	(bit_set :pointer)
	(user :pointer)
	(clip_box FT_BBox))

(cffi:defcstruct FT_Raster_Funcs
	(glyph_format FT_Glyph_Format)
	(raster_new :pointer)
	(raster_reset :pointer)
	(raster_set_mode :pointer)
	(raster_render :pointer)
	(raster_done :pointer))

(cffi:defcstruct FT_UnitVector
	(x :short)
	(y :short))

(cffi:defcstruct FT_Matrix
	(xx :long)
	(xy :long)
	(yx :long)
	(yy :long))

(cffi:defcstruct FT_Data
	(pointer :pointer)
	(length :int))

(cffi:defcstruct FT_Generic
	(data :pointer)
	(finalizer :pointer))

(cffi:defcstruct FT_ListNodeRec
	(prev :pointer)
	(next :pointer)
	(data :pointer))

(cffi:defcstruct FT_ListRec
	(head :pointer)
	(tail :pointer))

(cffi:defcstruct FT_Glyph_Metrics
	(width :long)
	(height :long)
	(horiBearingX :long)
	(horiBearingY :long)
	(horiAdvance :long)
	(vertBearingX :long)
	(vertBearingY :long)
	(vertAdvance :long))

(cffi:defcstruct FT_Bitmap_Size
	(height :short)
	(width :short)
	(size :long)
	(x_ppem :long)
	(y_ppem :long))

(cffi:defcstruct FT_CharMapRec
	(face :pointer)
	(encoding :int)
	(platform_id :unsigned-short)
	(encoding_id :unsigned-short))

(cffi:defcstruct FT_FaceRec
	(num_faces :long)
	(face_index :long)
	(face_flags :long)
	(style_flags :long)
	(num_glyphs :long)
	(family_name :string)
	(style_name :string)
	(num_fixed_sizes :int)
	(available_sizes :pointer)
	(num_charmaps :int)
	(charmaps :pointer)
	(generic FT_Generic)
	(bbox FT_BBox)
	(units_per_EM :unsigned-short)
	(ascender :short)
	(descender :short)
	(height :short)
	(max_advance_width :short)
	(max_advance_height :short)
	(underline_position :short)
	(underline_thickness :short)
	(glyph :pointer)
	(size :pointer)
	(charmap :pointer)
	(driver :pointer)
	(memory :pointer)
	(stream :pointer)
	(sizes_list FT_ListRec)
	(autohint FT_Generic)
	(extensions :pointer)
	(internal :pointer))

#|
(cl:defconstant FT_FACE_FLAG_SCALABLE 1L << 0)

(cl:defconstant FT_FACE_FLAG_FIXED_SIZES 1L << 1)

(cl:defconstant FT_FACE_FLAG_FIXED_WIDTH 1L << 2)

(cl:defconstant FT_FACE_FLAG_SFNT 1L << 3)

(cl:defconstant FT_FACE_FLAG_HORIZONTAL 1L << 4)

(cl:defconstant FT_FACE_FLAG_VERTICAL 1L << 5)

(cl:defconstant FT_FACE_FLAG_KERNING 1L << 6)

(cl:defconstant FT_FACE_FLAG_FAST_GLYPHS 1L << 7)

(cl:defconstant FT_FACE_FLAG_MULTIPLE_MASTERS 1L << 8)

(cl:defconstant FT_FACE_FLAG_GLYPH_NAMES 1L << 9)

(cl:defconstant FT_FACE_FLAG_EXTERNAL_STREAM 1L << 10)

(cl:defconstant FT_FACE_FLAG_HINTER 1L << 11)

(cl:defconstant FT_FACE_FLAG_CID_KEYED 1L << 12)

(cl:defconstant FT_FACE_FLAG_TRICKY 1L << 13)
|#

(cl:defconstant FT_STYLE_FLAG_ITALIC (cl:ash 1 0))

(cl:defconstant FT_STYLE_FLAG_BOLD (cl:ash 1 1))

(cffi:defcstruct FT_Size_Metrics
	(x_ppem :unsigned-short)
	(y_ppem :unsigned-short)
	(x_scale :long)
	(y_scale :long)
	(ascender :long)
	(descender :long)
	(height :long)
	(max_advance :long))

(cffi:defcstruct FT_SizeRec
	(face :pointer)
	(generic FT_Generic)
	(metrics FT_Size_Metrics)
	(internal :pointer))

(cffi:defcstruct FT_GlyphSlotRec
	(library :pointer)
	(face :pointer)
	(next :pointer)
	(reserved :unsigned-int)
	(generic FT_Generic)
	(metrics FT_Glyph_Metrics)
	(linearHoriAdvance :long)
	(linearVertAdvance :long)
	(advance FT_Vector)
	(format FT_Glyph_Format)
	(bitmap FT_Bitmap)
	(bitmap_left :int)
	(bitmap_top :int)
	(outline FT_Outline)
	(num_subglyphs :unsigned-int)
	(subglyphs :pointer)
	(control_data :pointer)
	(control_len :long)
	(lsb_delta :long)
	(rsb_delta :long)
	(other :pointer)
	(internal :pointer))

(cffi:defcfun ("FT_Init_FreeType" FT_Init_FreeType) :int
  (alibrary :pointer))

(cffi:defcfun ("FT_Done_FreeType" FT_Done_FreeType) :int
  (library :pointer))

(cl:defconstant FT_OPEN_MEMORY #x1)

(cl:defconstant FT_OPEN_STREAM #x2)

(cl:defconstant FT_OPEN_PATHNAME #x4)

(cl:defconstant FT_OPEN_DRIVER #x8)

(cl:defconstant FT_OPEN_PARAMS #x10)

(cl:defconstant ft_open_memory #x1)

(cl:defconstant ft_open_stream #x2)

(cl:defconstant ft_open_pathname #x4)

(cl:defconstant ft_open_driver #x8)

(cl:defconstant ft_open_params #x10)

(cffi:defcstruct FT_Parameter
	(tag :unsigned-long)
	(data :pointer))

(cffi:defcstruct FT_Open_Args
	(flags :unsigned-int)
	(memory_base :pointer)
	(memory_size :long)
	(pathname :string)
	(stream :pointer)
	(driver :pointer)
	(num_params :int)
	(params :pointer))

(cffi:defcfun ("FT_New_Face" FT_New_Face) :int
  (library :pointer)
  (filepathname :string)
  (face_index :long)
  (aface :pointer))

(cffi:defcfun ("FT_New_Memory_Face" FT_New_Memory_Face) :int
  (library :pointer)
  (file_base :pointer)
  (file_size :long)
  (face_index :long)
  (aface :pointer))

(cffi:defcfun ("FT_Open_Face" FT_Open_Face) :int
  (library :pointer)
  (args :pointer)
  (face_index :long)
  (aface :pointer))

(cffi:defcfun ("FT_Attach_File" FT_Attach_File) :int
  (face :pointer)
  (filepathname :string))

(cffi:defcfun ("FT_Attach_Stream" FT_Attach_Stream) :int
  (face :pointer)
  (parameters :pointer))

(cffi:defcfun ("FT_Done_Face" FT_Done_Face) :int
  (face :pointer))

(cffi:defcfun ("FT_Select_Size" FT_Select_Size) :int
  (face :pointer)
  (strike_index :int))

(cffi:defcenum FT_Size_Request_Type
	:FT_SIZE_REQUEST_TYPE_NOMINAL
	:FT_SIZE_REQUEST_TYPE_REAL_DIM
	:FT_SIZE_REQUEST_TYPE_BBOX
	:FT_SIZE_REQUEST_TYPE_CELL
	:FT_SIZE_REQUEST_TYPE_SCALES
	:FT_SIZE_REQUEST_TYPE_MAX)

(cffi:defcstruct FT_Size_RequestRec
	(type FT_Size_Request_Type)
	(width :long)
	(height :long)
	(horiResolution :unsigned-int)
	(vertResolution :unsigned-int))

(cffi:defcfun ("FT_Request_Size" FT_Request_Size) :int
  (face :pointer)
  (req :pointer))

(cffi:defcfun ("FT_Set_Char_Size" FT_Set_Char_Size) :int
  (face :pointer)
  (char_width :long)
  (char_height :long)
  (horz_resolution :unsigned-int)
  (vert_resolution :unsigned-int))

(cffi:defcfun ("FT_Set_Pixel_Sizes" FT_Set_Pixel_Sizes) :int
  (face :pointer)
  (pixel_width :unsigned-int)
  (pixel_height :unsigned-int))

(cffi:defcfun ("FT_Load_Glyph" FT_Load_Glyph) :int
  (face :pointer)
  (glyph_index :unsigned-int)
  (load_flags :int))

(cffi:defcfun ("FT_Load_Char" FT_Load_Char) :int
  (face :pointer)
  (char_code :unsigned-long)
  (load_flags :int))

(cl:defconstant FT_LOAD_DEFAULT #x0)

(cl:defconstant FT_LOAD_NO_SCALE #x1)

(cl:defconstant FT_LOAD_NO_HINTING #x2)

(cl:defconstant FT_LOAD_RENDER #x4)

(cl:defconstant FT_LOAD_NO_BITMAP #x8)

(cl:defconstant FT_LOAD_VERTICAL_LAYOUT #x10)

(cl:defconstant FT_LOAD_FORCE_AUTOHINT #x20)

(cl:defconstant FT_LOAD_CROP_BITMAP #x40)

(cl:defconstant FT_LOAD_PEDANTIC #x80)

(cl:defconstant FT_LOAD_IGNORE_GLOBAL_ADVANCE_WIDTH #x200)

(cl:defconstant FT_LOAD_NO_RECURSE #x400)

(cl:defconstant FT_LOAD_IGNORE_TRANSFORM #x800)

(cl:defconstant FT_LOAD_MONOCHROME #x1000)

(cl:defconstant FT_LOAD_LINEAR_DESIGN #x2000)

(cl:defconstant FT_LOAD_NO_AUTOHINT #x8000)

(cl:defconstant FT_LOAD_ADVANCE_ONLY #x100)

(cl:defconstant FT_LOAD_SBITS_ONLY #x4000)

(cffi:defcfun ("FT_Set_Transform" FT_Set_Transform) :void
  (face :pointer)
  (matrix :pointer)
  (delta :pointer))

(cffi:defcenum FT_Render_Mode
	(:FT_RENDER_MODE_NORMAL 0)
	:FT_RENDER_MODE_LIGHT
	:FT_RENDER_MODE_MONO
	:FT_RENDER_MODE_LCD
	:FT_RENDER_MODE_LCD_V
	:FT_RENDER_MODE_MAX)

(cffi:defcfun ("FT_Render_Glyph" FT_Render_Glyph) :int
  (slot :pointer)
  (render_mode FT_Render_Mode))

(cffi:defcenum FT_Kerning_Mode
	(:FT_KERNING_DEFAULT 0)
	:FT_KERNING_UNFITTED
	:FT_KERNING_UNSCALED)

(cffi:defcfun ("FT_Get_Kerning" FT_Get_Kerning) :int
  (face :pointer)
  (left_glyph :unsigned-int)
  (right_glyph :unsigned-int)
  (kern_mode :unsigned-int)
  (akerning :pointer))

(cffi:defcfun ("FT_Get_Track_Kerning" FT_Get_Track_Kerning) :int
  (face :pointer)
  (point_size :long)
  (degree :int)
  (akerning :pointer))

(cffi:defcfun ("FT_Get_Glyph_Name" FT_Get_Glyph_Name) :int
  (face :pointer)
  (glyph_index :unsigned-int)
  (buffer :pointer)
  (buffer_max :unsigned-int))

(cffi:defcfun ("FT_Get_Postscript_Name" FT_Get_Postscript_Name) :string
  (face :pointer))

(cffi:defcfun ("FT_Select_Charmap" FT_Select_Charmap) :int
  (face :pointer)
  (encoding :int))

(cffi:defcfun ("FT_Set_Charmap" FT_Set_Charmap) :int
  (face :pointer)
  (charmap :pointer))

(cffi:defcfun ("FT_Get_Charmap_Index" FT_Get_Charmap_Index) :int
  (charmap :pointer))

(cffi:defcfun ("FT_Get_Char_Index" FT_Get_Char_Index) :unsigned-int
  (face :pointer)
  (charcode :unsigned-long))

(cffi:defcfun ("FT_Get_First_Char" FT_Get_First_Char) :unsigned-long
  (face :pointer)
  (agindex :pointer))

(cffi:defcfun ("FT_Get_Next_Char" FT_Get_Next_Char) :unsigned-long
  (face :pointer)
  (char_code :unsigned-long)
  (agindex :pointer))

(cffi:defcfun ("FT_Get_Name_Index" FT_Get_Name_Index) :unsigned-int
  (face :pointer)
  (glyph_name :string))

(cl:defconstant FT_SUBGLYPH_FLAG_ARGS_ARE_WORDS 1)

(cl:defconstant FT_SUBGLYPH_FLAG_ARGS_ARE_XY_VALUES 2)

(cl:defconstant FT_SUBGLYPH_FLAG_ROUND_XY_TO_GRID 4)

(cl:defconstant FT_SUBGLYPH_FLAG_SCALE 8)

(cl:defconstant FT_SUBGLYPH_FLAG_XY_SCALE #x40)

(cl:defconstant FT_SUBGLYPH_FLAG_2X2 #x80)

(cl:defconstant FT_SUBGLYPH_FLAG_USE_MY_METRICS #x200)

(cffi:defcfun ("FT_Get_SubGlyph_Info" FT_Get_SubGlyph_Info) :int
  (glyph :pointer)
  (sub_index :unsigned-int)
  (p_index :pointer)
  (p_flags :pointer)
  (p_arg1 :pointer)
  (p_arg2 :pointer)
  (p_transform :pointer))

(cl:defconstant FT_FSTYPE_INSTALLABLE_EMBEDDING #x0000)

(cl:defconstant FT_FSTYPE_RESTRICTED_LICENSE_EMBEDDING #x0002)

(cl:defconstant FT_FSTYPE_PREVIEW_AND_PRINT_EMBEDDING #x0004)

(cl:defconstant FT_FSTYPE_EDITABLE_EMBEDDING #x0008)

(cl:defconstant FT_FSTYPE_NO_SUBSETTING #x0100)

(cl:defconstant FT_FSTYPE_BITMAP_EMBEDDING_ONLY #x0200)

(cffi:defcfun ("FT_Get_FSType_Flags" FT_Get_FSType_Flags) :unsigned-short
  (face :pointer))

(cffi:defcfun ("FT_Face_GetCharVariantIndex" FT_Face_GetCharVariantIndex) :unsigned-int
  (face :pointer)
  (charcode :unsigned-long)
  (variantSelector :unsigned-long))

(cffi:defcfun ("FT_Face_GetCharVariantIsDefault" FT_Face_GetCharVariantIsDefault) :int
  (face :pointer)
  (charcode :unsigned-long)
  (variantSelector :unsigned-long))

(cffi:defcfun ("FT_Face_GetVariantSelectors" FT_Face_GetVariantSelectors) :pointer
  (face :pointer))

(cffi:defcfun ("FT_Face_GetVariantsOfChar" FT_Face_GetVariantsOfChar) :pointer
  (face :pointer)
  (charcode :unsigned-long))

(cffi:defcfun ("FT_Face_GetCharsOfVariant" FT_Face_GetCharsOfVariant) :pointer
  (face :pointer)
  (variantSelector :unsigned-long))

(cffi:defcfun ("FT_MulDiv" FT_MulDiv) :long
  (a :long)
  (b :long)
  (c :long))

(cffi:defcfun ("FT_MulFix" FT_MulFix) :long
  (a :long)
  (b :long))

(cffi:defcfun ("FT_DivFix" FT_DivFix) :long
  (a :long)
  (b :long))

(cffi:defcfun ("FT_RoundFix" FT_RoundFix) :long
  (a :long))

(cffi:defcfun ("FT_CeilFix" FT_CeilFix) :long
  (a :long))

(cffi:defcfun ("FT_FloorFix" FT_FloorFix) :long
  (a :long))

(cffi:defcfun ("FT_Vector_Transform" FT_Vector_Transform) :void
  (vec :pointer)
  (matrix :pointer))

(cl:defconstant FREETYPE_MAJOR 2)

(cl:defconstant FREETYPE_MINOR 3)

(cl:defconstant FREETYPE_PATCH 9)

(cffi:defcfun ("FT_Library_Version" FT_Library_Version) :void
  (library :pointer)
  (amajor :pointer)
  (aminor :pointer)
  (apatch :pointer))

(cffi:defcfun ("FT_Face_CheckTrueTypePatents" FT_Face_CheckTrueTypePatents) :unsigned-char
  (face :pointer))

(cffi:defcfun ("FT_Face_SetUnpatentedHinting" FT_Face_SetUnpatentedHinting) :unsigned-char
  (face :pointer)
  (value :unsigned-char))


