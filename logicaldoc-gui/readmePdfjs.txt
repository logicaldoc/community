- Move pdf.js and pdf.worker.js and .map counterparts into the root

In viewer.mjs:
Replace ../build/ with ./
Replace ../web/ with ./
Replace sidebarViewOnLoad: -1, with sidebarViewOnLoad: false, 
   

Comment
defaultUrl: {
    value: 'compressed.tracemonkey-pldi-09.pdf',
    kind: OptionKind.VIEWER
  },    
   
   
In viewer.html: 
Replace ../build/ with ./
Replace ../web/ with ./ 
Replace <title>PDF.js viewer</title> with <title>Preview</title>

    
- copy the HTML part of viewer.html into index.jsp

 