package com.logicaldoc.core.parser;

import java.io.File;
import java.io.FileInputStream;
import java.io.InputStream;
import java.io.StringReader;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.Attributes;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;
import org.xml.sax.helpers.DefaultHandler;

import com.logicaldoc.util.StringUtil;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.ZipUtil;

/**
 * Text extractor for KOffice 1.6 documents.
 * 
 * @author Alessandro Gasparini - LogicalDOC
 * @since 4.5.2
 */
public class KOfficeParser extends AbstractParser {

	private static final Logger log = LoggerFactory.getLogger(KOfficeParser.class);

	// --------------------------------------------< KOfficeContentHandler >

	private class KOfficeContentHandler extends DefaultHandler {

		private StringBuilder content;

		private boolean appendChar;

		public KOfficeContentHandler() {
			content = new StringBuilder();
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
			if (rawName.equalsIgnoreCase("TEXT")) {
				appendChar = true;
			}
		}

		@Override
		public void characters(char[] ch, int start, int length) throws SAXException {
			if (appendChar) {
				content.append(ch, start, length).append(" ");
			}
		}

		@Override
		public void endElement(java.lang.String namespaceURI, java.lang.String localName, java.lang.String qName)
				throws SAXException {
			appendChar = false;
		}
	}

	@Override
	public void internalParse(InputStream input, ParseParameters parameters, StringBuilder content) {
		try {
			SAXParserFactory factory = SAXParserFactory.newInstance();
			// to be compliant, completely disable DOCTYPE declaration:
			factory.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
			factory.setValidating(false);
			SAXParser saxParser = factory.newSAXParser();
			XMLReader xmlReader = saxParser.getXMLReader();
			xmlReader.setFeature("http://xml.org/sax/features/validation", false);
			xmlReader.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);

			File mainDocXml = FileUtil.createTempFile("koffice-maindoc", ".xml");
			try (ZipUtil zipUtil = new ZipUtil()) {
				if (zipUtil.unzip(input, "maindoc.xml", mainDocXml) > 0) {
					try (InputStream mainDocStream = new FileInputStream(mainDocXml)) {
						KOfficeContentHandler contentHandler = new KOfficeContentHandler();
						xmlReader.setContentHandler(contentHandler);
						xmlReader.parse(new InputSource(mainDocStream));
						content.append(StringUtil.writeToString(new StringReader(contentHandler.getContent())));
					}
				}
			} finally {
				FileUtil.delete(mainDocXml);
			}
		} catch (Exception e) {
			log.warn("Failed to extract KOffice text content", e);
		}
	}
}
