package com.logicaldoc.core.parser;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.StringReader;
import java.util.Locale;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.StringUtil;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.IOUtil;
import com.logicaldoc.util.io.ZipUtil;

/**
 * Text extractor for OpenOffice/OpenDocument documents.
 * 
 * Tested with OpenOffice documents produced with 2.3,2.4,3.0.1 OO release Works
 * with extensions (odt, ods, odp) and templates (ott, ots, otp). Tested with
 * StarOffice documents (sxw, sxc, sxi)
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 4.5.2
 */
public class OpenOfficeParser extends AbstractParser {

	protected static Logger log = LoggerFactory.getLogger(OpenOfficeParser.class);

	/**
	 * XML handler to extract the texts crom the ODF content.xml
	 * 
	 * @author Marco Meschieri - LogicalDOC
	 * @since 8.6.1
	 */
	private class OpenOfficeContentHandler extends DefaultHandler {

		private StringBuffer content;

		private boolean appendChar;

		private boolean appendNewline;

		public OpenOfficeContentHandler() {
			content = new StringBuffer();
			appendChar = false;
		}

		/**
		 * Returns the text content extracted from parsed content.xml
		 */
		public String getContent() {
			return content.toString();
		}

		@Override
		public void startElement(String namespaceURI, String localName, String rawName, Attributes atts)
				throws SAXException {
			if (rawName.startsWith("text:"))
				appendChar = true;
			if (rawName.startsWith("text:p") || rawName.startsWith("text:index-title")
					|| rawName.startsWith("text:title") || rawName.startsWith("text:list-item"))
				appendNewline = true;
		}

		@Override
		public void characters(char[] ch, int start, int length) throws SAXException {
			if (appendNewline) {
				content.append("\n");
				appendNewline = false;
			}

			if (appendChar) {
				content.append(ch, start, length);
			}
		}

		public void endElement(java.lang.String namespaceURI, java.lang.String localName, java.lang.String qName)
				throws SAXException {
			appendChar = false;
			appendNewline = false;
		}
	}

	/**
	 * XML handler to extract the pages stats from the meta.xml
	 * 
	 * @author Marco Meschieri - LogicalDOC
	 * @since 8.6.1
	 */
	private class OpenOfficeMetadataHandler extends DefaultHandler {
		int pages = -1;

		public int getPages() {
			return pages;
		}

		@Override
		public void startElement(String namespaceURI, String localName, String rawName, Attributes atts)
				throws SAXException {
			if (rawName.toLowerCase().contains("document-statistic")) {
				// Look for the presence of the page-count attribute
				for (int i = 0; i < atts.getLength(); i++) {
					if (atts.getQName(i).toLowerCase().contains("page-count")) {
						pages = Integer.parseInt(atts.getValue(i));
						return;
					}
				}

				// Look for the presence of the table-count attribute (typical
				// in ods sheets)
				for (int i = 0; i < atts.getLength(); i++) {
					if (atts.getQName(i).toLowerCase().contains("table-count")) {
						pages = Integer.parseInt(atts.getValue(i));
						return;
					}
				}
			}
		}

		@Override
		public void characters(char[] ch, int start, int length) throws SAXException {
			// Nothing to do
		}

		public void endElement(java.lang.String namespaceURI, java.lang.String localName, java.lang.String qName)
				throws SAXException {
			// Nothing to do
		}
	}

	/**
	 * XML handler to extract the slides of a presentation
	 * 
	 * @author Marco Meschieri - LogicalDOC
	 * @since 8.6.1
	 */
	private class OpenOfficePresentationMetadataHandler extends DefaultHandler {
		int pages = -1;

		public int getPages() {
			return pages;
		}

		@Override
		public void startElement(String namespaceURI, String localName, String rawName, Attributes atts)
				throws SAXException {
			if (rawName.toLowerCase().endsWith(":page")) {
				if (pages == -1)
					pages = 1;
				else
					pages++;
			}
		}

		@Override
		public void characters(char[] ch, int start, int length) throws SAXException {
			// Nothing to do
		}

		public void endElement(java.lang.String namespaceURI, java.lang.String localName, java.lang.String qName)
				throws SAXException {
			// Nothing to do
		}
	}

	@Override
	public void internalParse(InputStream input, String filename, String encoding, Locale locale, String tenant,
			Document document, String fileVersion, StringBuffer content) {
		try {
			SAXParserFactory factory = SAXParserFactory.newInstance();
			factory.setFeature("http://xml.org/sax/features/external-general-entities", false);
			factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
			factory.setValidating(false);

			SAXParser saxParser = factory.newSAXParser();
			XMLReader xmlReader = saxParser.getXMLReader();
			xmlReader.setFeature("http://xml.org/sax/features/validation", false);
			xmlReader.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);

			File contentXml = File.createTempFile("openoffice-content", ".xml");
			try {
				if (new ZipUtil().unzip(input, "content.xml", contentXml) > 0) {
					try (InputStream contentStream = new FileInputStream(contentXml)) {
						OpenOfficeContentHandler contentHandler = new OpenOfficeContentHandler();
						xmlReader.setContentHandler(contentHandler);
						xmlReader.parse(new InputSource(contentStream));
						content.append(StringUtil.writeToString(new StringReader(contentHandler.getContent())));
					}
				}
			} finally {
				FileUtil.strongDelete(contentXml);
			}
		} catch (Throwable e) {
			log.warn("Failed to extract OpenOffice text content", e);
		}
	}

	@Override
	public int countPages(InputStream input, String filename) {
		File tmp = null;
		try {
			tmp = File.createTempFile("countpages", null);
			IOUtil.write(input, tmp);
			return countPages(tmp, filename);
		} catch (Throwable e) {
			log.warn("Failed to extract OpenOffice metadata", e);
			return 1;
		}
	}

	@Override
	public int countPages(File input, String filename) {
		try {
			SAXParserFactory factory = SAXParserFactory.newInstance();
			factory.setFeature("http://xml.org/sax/features/external-general-entities", false);
			factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
			factory.setValidating(false);
			SAXParser saxParser = factory.newSAXParser();
			XMLReader xmlReader = saxParser.getXMLReader();
			xmlReader.setFeature("http://xml.org/sax/features/validation", false);
			xmlReader.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);

			int pages = 1;
			ZipUtil zipUtil = new ZipUtil();
			try (InputStream is = zipUtil.getEntryStream(input, "meta.xml")) {
				OpenOfficeMetadataHandler metadataHandler = new OpenOfficeMetadataHandler();
				xmlReader.setContentHandler(metadataHandler);
				xmlReader.parse(new InputSource(is));
				if (metadataHandler.getPages() > pages)
					pages = metadataHandler.getPages();
			} catch (Throwable t) {
				log.warn("Failed to extract OpenOffice meta.xml entry", t);
			}

			try (InputStream is = zipUtil.getEntryStream(input, "content.xml")) {
				OpenOfficePresentationMetadataHandler metadataHandler = new OpenOfficePresentationMetadataHandler();
				xmlReader.setContentHandler(metadataHandler);
				xmlReader.parse(new InputSource(is));
				if (metadataHandler.getPages() > pages)
					pages = metadataHandler.getPages();
			} catch (Throwable t) {
				log.warn("Failed to extract OpenOffice content.xml entry", t);
			}

			return pages;
		} catch (Throwable e) {
			log.warn("Failed to extract OpenOffice metadata", e);
			return 1;
		}
	}
}
