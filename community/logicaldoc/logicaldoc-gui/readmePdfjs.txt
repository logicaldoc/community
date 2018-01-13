- Move pdf.js and pdf.worker.js into the root

- Edit pdf.worker.js and comment the line:
var DEFAULT_URL = 'compressed.tracemonkey-pldi-09.pdf';

- Search the reference to pdf.worker.js and replace with this configuration:
function configure(PDFJS) {
  PDFJS.imageResourcesPath = './images/';
  PDFJS.workerSrc = './pdf.worker.js';
  PDFJS.cMapUrl = './cmaps/';
  PDFJS.cMapPacked = true;
}

- copy the HTML part of viewer.html into index.jsp

 