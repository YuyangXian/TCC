pro Calculate_TCC

  compile_opt idl2
  ENVI,/restore_base_save_files
  ENVI_batch_init,NO_STATUS_WINDOW=1

  TIC

  inpath = 'G:\QZ_TCC_product_input_data\4_rugged_area\'
  file_search1=file_search(inpath,'*.tif',count = num,/test_regular)

  for i = 0, num-1 do begin

    file_DEM=file_search1[i]
    TCCSDRresult=Cal_TCCSDR(file_DEM)
    TCCDIFresult=Cal_TCCDIF(file_DEM)
    TCCREFresult=Cal_TCCREF(file_DEM)
    uTCCSDRresult=upscale_TCCSDR(DEM_file)
    uTCCDIFresult=upscale_TCCDIF(DEM_file)
    uTCCREFresult=upscale_TCCREF(DEM_file)

  endfor

  print,'finish'

  TOC

end;


function Cal_TCCSDR,file_DEM

  fname=FILE_BASENAME(file_DEM, '.tif')
  fname=STRMID(fname,12)

  DEM_initial_01='G:\QZ_TCC_product_input_data\6_DEM\srtm_' +strcompress(fname, /remove)+'.tif'
  DEM_01=read_tiff(DEM_initial_01,geotiff=geotiff)
  DEM_01=[]

  rugged_area=READ_TIFF(file_DEM)/10.

  file_SVF='G:\QZ_TCC_product_input_data\3_SVF\SVF_' +strcompress(fname, /remove)+'.tif'
  SVF_original=READ_TIFF(file_SVF)

  SVF=SVF_original
  SVF[where(~finite(SVF))] = 1

  file_slope='G:\QZ_TCC_product_input_data\2_slope\slope_' +strcompress(fname, /remove)+'.tif'
  slope_original=READ_TIFF(file_slope)*!DTOR

  file_aspect='G:\QZ_TCC_product_input_data\1_aspect\aspect_' +strcompress(fname, /remove)+'.tif'
  aspect_original=READ_TIFF(file_aspect)*!DTOR

  slope=slope_original
  aspect=aspect_original
  slope[where(~finite(slope))] = 0
  aspect[where(~finite(aspect))] = 0

  FOR SZA = 5, 90, 5 DO BEGIN

    FOR SAA = 0, 355, 5 DO BEGIN

      out_file='F:\TCCSDR\TCCSDR_' +strcompress(fname, /remove)+'_'+strcompress(SZA, /remove)+'_'+strcompress(SAA, /remove)+'.tif'
      result_TCC=file_test(out_file)

      if (result_TCC eq 0) then begin

        SZA1=SZA*!DTOR
        SAA1=SAA*!DTOR

        file_shadow='G:\QZ_TCC_product_input_data\5_shadow\shadow_'+strcompress(fname, /remove)+'_'+strcompress(SZA, /remove)+'_'+strcompress(SAA, /remove)+'.tif'
        shadow=read_tiff(file_shadow)

        COSIS=COS(SZA1)*COS(slope)+sin(SZA1)*sin(slope)*cos(SAA1-aspect)

        area_atio=900/(cos(slope)*rugged_area)<1

        TCCSDR=area_atio*SVF*shadow*COSIS/cos(SZA1)

        TCCSDR[where(~finite(slope_original))] = 1
        TCCSDR=TCCSDR*1000

        out_file='F:\TCCSDR\TCCSDR_'+strcompress(fname, /remove)+'_'+strcompress(SZA, /remove)+'_'+strcompress(SAA, /remove)+'.tif'

        WRITE_TIFF,out_file,TCCSDR,/short,/signed,geotiff=geotiff,COMPRESSION=1

        print,out_file+'  '+STRTRIM(STRING(max(TCCSDR)),2)

      ENDIF

    ENDFOR

  ENDFOR

end



function Cal_TCCDIF,file_DEM

  fname=FILE_BASENAME(file_DEM, '.tif')
  fname=STRMID(fname,12)

  out_file='F:\TCCDIF\TCCDIF_' +strcompress(fname, /remove)+'.tif'
  result_TCC=file_test(out_file)

  if (result_TCC eq 0) then begin

    DEM_initial_01='G:\QZ_TCC_product_input_data\6_DEM\srtm_' +strcompress(fname, /remove)+'.tif'
    DEM_01=read_tiff(DEM_initial_01,geotiff=geotiff)
    DEM_01=[]

    rugged_area=READ_TIFF(file_DEM)/10.

    file_SVF='G:\QZ_TCC_product_input_data\3_SVF\SVF_' +strcompress(fname, /remove)+'.tif'
    SVF_original=READ_TIFF(file_SVF)

    SVF=SVF_original
    SVF[where(~finite(SVF))] = 1


    file_slope='G:\QZ_TCC_product_input_data\2_slope\slope_' +strcompress(fname, /remove)+'.tif'
    slope_original=READ_TIFF(file_slope)*!DTOR

    file_aspect='G:\QZ_TCC_product_input_data\1_aspect\aspect_' +strcompress(fname, /remove)+'.tif'
    aspect_original=READ_TIFF(file_aspect)*!DTOR

    slope=slope_original
    aspect=aspect_original
    slope[where(~finite(slope))] = 0
    aspect[where(~finite(aspect))] = 0

    area_atio=900/(cos(slope)*rugged_area)<1

    TCCDIF=area_atio*SVF

    TCCDIF[where(~finite(slope_original))] = 1
    TCCDIF=TCCDIF*1000

    out_file='F:\TCCDIF\TCCDIF_' +strcompress(fname, /remove)+'.tif'

    WRITE_TIFF,out_file,TCCDIF,/short,/signed,geotiff=geotiff,COMPRESSION=1

    print,out_file+'  '+STRTRIM(STRING(max(TCCDIF)),2)

  ENDIF

end


function Cal_TCCREF,file_DEM

  fname=FILE_BASENAME(file_DEM, '.tif')
  fname=STRMID(fname,12)

  out_file='F:\TCCREF\TCCREF_' +strcompress(fname, /remove)+'.tif'
  result_TCC=file_test(out_file)

  if (result_TCC eq 0) then begin

    DEM_initial_01='G:\QZ_TCC_product_input_data\6_DEM\srtm_' +strcompress(fname, /remove)+'.tif'
    DEM_01=read_tiff(DEM_initial_01,geotiff=geotiff)
    DEM_01=[]

    rugged_area=READ_TIFF(file_DEM)/10.

    file_SVF='G:\QZ_TCC_product_input_data\3_SVF\SVF_' +strcompress(fname, /remove)+'.tif'
    SVF_original=READ_TIFF(file_SVF)

    SVF=SVF_original
    SVF[where(~finite(SVF))] = 1

    TCCREF=1-SVF

    TCCREF[where(~finite(SVF_original))] = 1
    TCCREF=TCCREF*1000

    out_file='F:\TCCREF\TCCREF_' +strcompress(fname, /remove)+'.tif'

    WRITE_TIFF,out_file,TCCREF,/short,/signed,geotiff=geotiff,COMPRESSION=1

    print,out_file+'  '+STRTRIM(STRING(max(TCCREF)),2)

  ENDIF

end



function upscale_TCCSDR,DEM_file

  fname=FILE_BASENAME(DEM_file, '.tif')
  fname=STRMID(fname,12)

  test_file='F:\TCCSDR_2.5min\TCCSDR_' +strcompress(fname, /remove)+'_2.5min.tif'
  file_test(test_file)

  if (TEST eq 0) then begin

    FOR SZA = 5, 90, 5 DO BEGIN

      FOR SAA = 0, 355, 5 DO BEGIN

        FINE_file='F:\TCCSDR\TCCSDR_' +strcompress(fname, /remove)+'_'+strcompress(SZA, /remove)+'_'+strcompress(SAA, /remove)+'.tif'

        geofile=read_tiff(FINE_file,geotiff=geotiff_geo)
        x=geotiff_geo.MODELPIXELSCALETAG[0]
        y=geotiff_geo.MODELPIXELSCALETAG[1]

        IF (X EQ Y) then begin

          Fine_resolution=DOUBLE(x)

          Envi_open_file,FINE_file,r_fid=fid_Fine
          envi_file_query,fid_Fine,dims=dims_Fine,ns=ns_Fine,nl=nl_Fine
          Fine_DATA=READ_TIFF(FINE_file)*1d

          Envi_open_file,DEM_file,r_fid=fid_DEM
          envi_file_query,fid_DEM,dims=dims_DEM,ns=ns_DEM,nl=nl_DEM
          DEM_DATA=READ_TIFF(DEM_file)*1d

          if (ns_DEM eq ns_Fine)and (nl_DEM eq nl_Fine) then begin

            coarse_resolution=DOUBLE(0.0008333)
            geotiff_geo.MODELPIXELSCALETAG=[coarse_resolution, coarse_resolution, 0.0000000000000000]
            out_file='F:\TCCSDR_3s\TCCSDR_' +strcompress(fname, /remove)+'_'+strcompress(SZA, /remove)+'_'+strcompress(SAA, /remove)+'_3s.tif'
            BB=cal_upscale_TCC(coarse_resolution,Fine_resolution,ns_Fine,nl_Fine,DEM_DATA,Fine_DATA,out_file,geotiff_geo)

            coarse_resolution=DOUBLE(0.0041667)
            geotiff_geo.MODELPIXELSCALETAG=[coarse_resolution, coarse_resolution, 0.0000000000000000]
            out_file='F:\TCCSDR_15s\TCCSDR_' +strcompress(fname, /remove)+'_'+strcompress(SZA, /remove)+'_'+strcompress(SAA, /remove)+'_15s.tif'
            BB=cal_upscale_TCC(coarse_resolution,Fine_resolution,ns_Fine,nl_Fine,DEM_DATA,Fine_DATA,out_file,geotiff_geo)

            coarse_resolution=DOUBLE(0.008333)
            geotiff_geo.MODELPIXELSCALETAG=[coarse_resolution, coarse_resolution, 0.0000000000000000]
            out_file='F:\TCCSDR_30s\TCCSDR_' +strcompress(fname, /remove)+'_'+strcompress(SZA, /remove)+'_'+strcompress(SAA, /remove)+'_30s.tif'
            BB=cal_upscale_TCC(coarse_resolution,Fine_resolution,ns_Fine,nl_Fine,DEM_DATA,Fine_DATA,out_file,geotiff_geo)

            coarse_resolution=DOUBLE(0.016666)
            geotiff_geo.MODELPIXELSCALETAG=[coarse_resolution, coarse_resolution, 0.0000000000000000]
            out_file='F:\TCCSDR_1min\TCCSDR_' +strcompress(fname, /remove)+'_'+strcompress(SZA, /remove)+'_'+strcompress(SAA, /remove)+'_1min.tif'
            BB=cal_upscale_TCC(coarse_resolution,Fine_resolution,ns_Fine,nl_Fine,DEM_DATA,Fine_DATA,out_file,geotiff_geo)

            coarse_resolution=DOUBLE(0.041667)
            geotiff_geo.MODELPIXELSCALETAG=[coarse_resolution, coarse_resolution, 0.0000000000000000]
            out_file='F:\TCCSDR_2.5min\TCCSDR_' +strcompress(fname, /remove)+'_'+strcompress(SZA, /remove)+'_'+strcompress(SAA, /remove)+'_2.5min.tif'
            BB=cal_upscale_TCC(coarse_resolution,Fine_resolution,ns_Fine,nl_Fine,DEM_DATA,Fine_DATA,out_file,geotiff_geo)

          endif

        endif

      ENDFOR

    ENDFOR

  ENDIF

end



function upscale_TCCDIF,DEM_file

  fname=FILE_BASENAME(DEM_file, '.tif')
  fname=STRMID(fname,12)

  test_file='F:\TCCDIF_2.5min\TCCDIF_' +strcompress(fname, /remove)+'_2.5min.tif'
  file_test(test_file)

  if (TEST eq 0) then begin

    FOR SZA = 5, 90, 5 DO BEGIN

      FOR SAA = 0, 355, 5 DO BEGIN

        FINE_file='F:\TCCDIF\TCCDIF_' +strcompress(fname, /remove)+'.tif'

        geofile=read_tiff(FINE_file,geotiff=geotiff_geo)
        x=geotiff_geo.MODELPIXELSCALETAG[0]
        y=geotiff_geo.MODELPIXELSCALETAG[1]

        IF (X EQ Y) then begin

          Fine_resolution=DOUBLE(x)

          Envi_open_file,FINE_file,r_fid=fid_Fine
          envi_file_query,fid_Fine,dims=dims_Fine,ns=ns_Fine,nl=nl_Fine
          Fine_DATA=READ_TIFF(FINE_file)*1d

          Envi_open_file,DEM_file,r_fid=fid_DEM
          envi_file_query,fid_DEM,dims=dims_DEM,ns=ns_DEM,nl=nl_DEM
          DEM_DATA=READ_TIFF(DEM_file)*1d

          if (ns_DEM eq ns_Fine)and (nl_DEM eq nl_Fine) then begin

            coarse_resolution=DOUBLE(0.0008333)
            geotiff_geo.MODELPIXELSCALETAG=[coarse_resolution, coarse_resolution, 0.0000000000000000]
            out_file='F:\TCCDIF_3s\TCCDIF_' +strcompress(fname, /remove)+'_3s.tif'
            BB=cal_upscale_TCC(coarse_resolution,Fine_resolution,ns_Fine,nl_Fine,DEM_DATA,Fine_DATA,out_file,geotiff_geo)

            coarse_resolution=DOUBLE(0.0041667)
            geotiff_geo.MODELPIXELSCALETAG=[coarse_resolution, coarse_resolution, 0.0000000000000000]
            out_file='F:\TCCDIF_15s\TCCDIF_' +strcompress(fname, /remove)+'_15s.tif'
            BB=cal_upscale_TCC(coarse_resolution,Fine_resolution,ns_Fine,nl_Fine,DEM_DATA,Fine_DATA,out_file,geotiff_geo)

            coarse_resolution=DOUBLE(0.008333)
            geotiff_geo.MODELPIXELSCALETAG=[coarse_resolution, coarse_resolution, 0.0000000000000000]
            out_file='F:\TCCDIF_30s\TCCDIF_' +strcompress(fname, /remove)+'_30s.tif'
            BB=cal_upscale_TCC(coarse_resolution,Fine_resolution,ns_Fine,nl_Fine,DEM_DATA,Fine_DATA,out_file,geotiff_geo)

            coarse_resolution=DOUBLE(0.016666)
            geotiff_geo.MODELPIXELSCALETAG=[coarse_resolution, coarse_resolution, 0.0000000000000000]
            out_file='F:\TCCDIF_1min\TCCDIF_' +strcompress(fname, /remove)+'_1min.tif'
            BB=cal_upscale_TCC(coarse_resolution,Fine_resolution,ns_Fine,nl_Fine,DEM_DATA,Fine_DATA,out_file,geotiff_geo)

            coarse_resolution=DOUBLE(0.041667)
            geotiff_geo.MODELPIXELSCALETAG=[coarse_resolution, coarse_resolution, 0.0000000000000000]
            out_file='F:\TCCDIF_2.5min\TCCDIF_' +strcompress(fname, /remove)+'_2.5min.tif'
            BB=cal_upscale_TCC(coarse_resolution,Fine_resolution,ns_Fine,nl_Fine,DEM_DATA,Fine_DATA,out_file,geotiff_geo)

          endif

        endif

      ENDFOR

    ENDFOR

  ENDIF

end








function upscale_TCCREF,DEM_file

  fname=FILE_BASENAME(DEM_file, '.tif')
  fname=STRMID(fname,12)

  test_file='F:\TCCREF_2.5min\TCCREF_' +strcompress(fname, /remove)+'_2.5min.tif'
  file_test(test_file)

  if (TEST eq 0) then begin

    FOR SZA = 5, 90, 5 DO BEGIN

      FOR SAA = 0, 355, 5 DO BEGIN

        FINE_file='F:\TCCREF\TCCREF_' +strcompress(fname, /remove)+'.tif'

        geofile=read_tiff(FINE_file,geotiff=geotiff_geo)
        x=geotiff_geo.MODELPIXELSCALETAG[0]
        y=geotiff_geo.MODELPIXELSCALETAG[1]

        IF (X EQ Y) then begin

          Fine_resolution=DOUBLE(x)

          Envi_open_file,FINE_file,r_fid=fid_Fine
          envi_file_query,fid_Fine,dims=dims_Fine,ns=ns_Fine,nl=nl_Fine
          Fine_DATA=READ_TIFF(FINE_file)*1d

          Envi_open_file,DEM_file,r_fid=fid_DEM
          envi_file_query,fid_DEM,dims=dims_DEM,ns=ns_DEM,nl=nl_DEM
          DEM_DATA=READ_TIFF(DEM_file)*1d

          if (ns_DEM eq ns_Fine)and (nl_DEM eq nl_Fine) then begin

            coarse_resolution=DOUBLE(0.0008333)
            geotiff_geo.MODELPIXELSCALETAG=[coarse_resolution, coarse_resolution, 0.0000000000000000]
            out_file='F:\TCCREF_3s\TCCREF_' +strcompress(fname, /remove)+'_3s.tif'
            BB=cal_upscale_TCC(coarse_resolution,Fine_resolution,ns_Fine,nl_Fine,DEM_DATA,Fine_DATA,out_file,geotiff_geo)

            coarse_resolution=DOUBLE(0.0041667)
            geotiff_geo.MODELPIXELSCALETAG=[coarse_resolution, coarse_resolution, 0.0000000000000000]
            out_file='F:\TCCREF_15s\TCCREF_' +strcompress(fname, /remove)+'_15s.tif'
            BB=cal_upscale_TCC(coarse_resolution,Fine_resolution,ns_Fine,nl_Fine,DEM_DATA,Fine_DATA,out_file,geotiff_geo)

            coarse_resolution=DOUBLE(0.008333)
            geotiff_geo.MODELPIXELSCALETAG=[coarse_resolution, coarse_resolution, 0.0000000000000000]
            out_file='F:\TCCREF_30s\TCCREF_' +strcompress(fname, /remove)+'_30s.tif'
            BB=cal_upscale_TCC(coarse_resolution,Fine_resolution,ns_Fine,nl_Fine,DEM_DATA,Fine_DATA,out_file,geotiff_geo)

            coarse_resolution=DOUBLE(0.016666)
            geotiff_geo.MODELPIXELSCALETAG=[coarse_resolution, coarse_resolution, 0.0000000000000000]
            out_file='F:\TCCREF_1min\TCCREF_' +strcompress(fname, /remove)+'_1min.tif'
            BB=cal_upscale_TCC(coarse_resolution,Fine_resolution,ns_Fine,nl_Fine,DEM_DATA,Fine_DATA,out_file,geotiff_geo)

            coarse_resolution=DOUBLE(0.041667)
            geotiff_geo.MODELPIXELSCALETAG=[coarse_resolution, coarse_resolution, 0.0000000000000000]
            out_file='F:\TCCREFF_2.5min\TCCREF_' +strcompress(fname, /remove)+'_2.5min.tif'
            BB=cal_upscale_TCC(coarse_resolution,Fine_resolution,ns_Fine,nl_Fine,DEM_DATA,Fine_DATA,out_file,geotiff_geo)

          endif

        endif

      ENDFOR

    ENDFOR

  ENDIF

end





function cal_upscale_TCC,coarse_resolution,Fine_resolution,ns_Fine,nl_Fine,DEM_DATA,Fine_DATA,out_file,geotiff_geo

  mm=coarse_resolution/Fine_resolution
  nn=coarse_resolution/Fine_resolution

  m=ceil(ns_Fine*Fine_resolution/coarse_resolution)
  n=ceil(nl_Fine*Fine_resolution/coarse_resolution)

  Coarse_DATA=DBLARR(m,n)

  FOR i =0.0,n-1,1 DO BEGIN
    ix=double(floor(0+nn*i))
    iy=double(floor(nn-1+nn*i))

    if iy gt nl_Fine-1 then iy=nl_Fine-1

    FOR j = 0.0,m-1,1 DO BEGIN

      jx=double(floor(0+mm*j))
      jy=double(floor(mm-1+mm*j))

      if jy gt ns_Fine-1 then jy=ns_Fine-1

      Coarse_01=DEM_DATA[jx:jy,ix:iy]*Fine_DATA[jx:jy,ix:iy]/ TOTAL(DEM_DATA[jx:jy,ix:iy])
      Coarse_DATA[j,i] = TOTAL(Coarse_01)

    ENDFOR
  ENDFOR

  WRITE_TIFF,out_file,Coarse_DATA,/short,/signed,geotiff=geotiff_geo,COMPRESSION=1

  print,out_file+'  '+STRTRIM(STRING(max(Coarse_DATA)),2)

end
