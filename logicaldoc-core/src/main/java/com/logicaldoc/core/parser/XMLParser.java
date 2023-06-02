package com.logicaldoc.core.parser;

import java.io.CharArrayReader;
import java.io.CharArrayWriter;
import java.io.FilterInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.StringReader;
import java.nio.charset.Charset;

import javax.xml.parsers.SAXParser;
import javax.xml.parsers.SAXParserFactory;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;
import org.xml.sax.EntityResolver;
import org.xml.sax.InputSource;
import org.xml.sax.SAXException;
import org.xml.sax.XMLReader;

import com.logicaldoc.util.StringUtil;

/**
 * Text extractor for XML documents. This class extracts the text content and
 * attribute values from XML documents.
 * <p>
 * This class can handle any XML-based format. However, it often makes sense to
 * use more specialized extractors that better understand the specific content
 * type.
 * 
 * @author Michael Scholz
 * @author Alessandro Gasparini - LogicalDOC
 * @since 3.5
 */
public class XMLParser extends AbstractParser {

	protected static Logger log = LoggerFactory.getLogger(XMLParser.class);

	@Override
	public void internalParse(InputStream input, ParseParameters parameters, StringBuilder content) {
		try {
			CharArrayWriter writer = new CharArrayWriter();
			ExtractorHandler handler = new ExtractorHandler(writer);

			SAXParserFactory factory = SAXParserFactory.newInstance();
			factory.setFeature("http://xml.org/sax/features/external-general-entities", false);
			factory.setFeature("http://xml.org/sax/features/external-parameter-entities", false);

			SAXParser parser = factory.newSAXParser();
			XMLReader reader = parser.getXMLReader();
			reader.setContentHandler(handler);
			reader.setErrorHandler(handler);

			// Exclude the entity resolver in order to fix the XXE vulnerability
			reader.setEntityResolver(new EntityResolver() {

				@Override
				public InputSource resolveEntity(String publicId, String systemId) throws SAXException, IOException {
					return new InputSource(new StringReader(""));
				}
			});

			// It is unspecified whether the XML parser closes the stream when
			// done parsing. To ensure that the stream gets closed just once,
			// we prevent the parser from closing it by catching the close()
			// call and explicitly close the stream in a finally block.
			InputSource source = new InputSource(new FilterInputStream(input) {
				@Override
				public void close() {
					// Nothing to do
				}
			});
			if (parameters.getEncoding() != null) {
				setEncoding(parameters.getEncoding(), source);
			}
			reader.parse(source);

			content.append(StringUtil.writeToString(new CharArrayReader(writer.toCharArray())));
		} catch (Exception e) {
			log.warn("Failed to extract XML text content", e);
		}
	}

	private void setEncoding(String encoding, InputSource source) {
		try {
			Charset.forName(encoding);
			source.setEncoding(encoding);
		} catch (Exception e) {
			log.warn("Unsupported encoding '{}', using default ({}) instead.", encoding,
					System.getProperty("file.encoding"));
		}
	}
}