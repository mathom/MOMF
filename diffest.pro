Function diffest
outarr=fltarr(4,364)
for id=1,364 do begin
    if id lt 10 then strid='00'+strn(id)
    if (id ge 10 and id lt 100) then strid='0'+strn(id)
    if id ge 100 then strid=strn(id)

    erom,d0,d1,d2,d3,d4,d5,d6,file='/CCD3/martinrk/MOMF/out/fitsrun-3/REL/STAR.'+strid
    f5=d5
    erom,d0,d1,d2,d3,d4,d5,d6,file='/CCD3/martinrk/MOMF/out/REL/STAR.'+strid
    diff=(f5-d5) ;
    med=median(f5)
    if med lt 20 then begin
       momdiff=moment(diff)
       print,'For '+strid+' median is '+strmid(strn(med),0,4)+',difference is '+ $
strmid(strn(1e3*momdiff(0)),0,6)+' pm '+strmid(strn(1e3*sqrt(momdiff(1))),0,4)+' mmag'
       outarr(*,id-1)=[id,med,momdiff(0),momdiff(1)]
    endif
endfor
return,outarr
end


