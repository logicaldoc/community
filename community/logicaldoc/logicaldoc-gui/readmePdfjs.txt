- Move pdf.js and pdf.worker.js and .map counterparts into the root

- Edit pdf.worker.js and comment the lines:
if (data.fieldType === 'Sig') {
  _this2.setFlags(_util.AnnotationFlag.HIDDEN);
}


In viewer.js:

Replace
cMapUrl: {
    value: '../web/cmaps/',
with
cMapUrl: {
    value: './cmaps/',
    
Replace    
workerSrc: {
    value: '../build/pdf.worker.js',    
with    
workerSrc: {
    value: './pdf.worker.js',
    
    
Comment
defaultUrl: {
    value: 'compressed.tracemonkey-pldi-09.pdf',
    kind: OptionKind.VIEWER
  },    
    
- copy the HTML part of viewer.html into index.jsp

 