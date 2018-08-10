package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;
import java.util.List;

import javax.xml.transform.Result;
import javax.xml.transform.Source;
import javax.xml.transform.Templates;
import javax.xml.transform.Transformer;
import javax.xml.transform.TransformerConfigurationException;
import javax.xml.transform.TransformerException;
import javax.xml.transform.TransformerFactory;
import javax.xml.transform.stream.StreamResult;
import javax.xml.transform.stream.StreamSource;

import org.apache.commons.io.FileUtils;
import org.apache.commons.io.FilenameUtils;
import org.jdom.Element;
import org.jdom.ProcessingInstruction;
import org.jdom.input.SAXBuilder;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.Context;
import com.logicaldoc.util.io.FileUtil;

/**
 * Converter to convert XML files
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.0
 */
public class XMLConverter extends AbstractFormatConverter {

	protected static Logger log = LoggerFactory.getLogger(XMLConverter.class);

	@Override
	public void internalConvert(String sid, Document document, File src, File dest) throws IOException {
		String destExt = FilenameUtils.getExtension(dest.getName()).toLowerCase();
		File xslt = null;
		String xsltOutFormat = null;
		try {
			// Parse the XML searching for a stylesheet
			SAXBuilder saxBuilder = new SAXBuilder();
			org.jdom.Document doc = saxBuilder.build(src);
			List<Object> contents = doc.getContent();
			for (Object content : contents) {
				if (content instanceof ProcessingInstruction) {
					ProcessingInstruction pi = (ProcessingInstruction) content;
					if ("text/xsl".equals(pi.getPseudoAttributeValue("type"))) {
						// Found a style sheet, download it and check the output
						// format
						xslt = File.createTempFile("xslt", ".xsl");
						FileUtils.copyURLToFile(new URL(pi.getPseudoAttributeValue("href")), xslt);
						org.jdom.Document xsltDoc = saxBuilder.build(xslt);
						Element root = xsltDoc.getRootElement();
						Element outputElem = root.getChild("output", root.getNamespace());
						if (outputElem != null)
							xsltOutFormat = outputElem.getAttributeValue("method").toLowerCase();
					}
				}
			}

			FormatConverterManager manager = (FormatConverterManager) Context.get().getBean(
					FormatConverterManager.class);
			if (xsltOutFormat == null) {
				FormatConverter converter = manager.getConverter("txt", destExt);
				if (converter == null)
					throw new IOException(String.format("Unable to find a converter from txt to %s", destExt));
				converter.convert(sid, document, src, dest);
			} else {
				try {
					// Create transformer factory
					TransformerFactory factory = TransformerFactory.newInstance();

					// Use the factory to create a template containing the
					// xsl
					// file
					Templates template = factory.newTemplates(new StreamSource(new FileInputStream(xslt)));

					// Use the template to create a transformer
					Transformer xformer = template.newTransformer();

					// Prepare the input file
					Source source = new StreamSource(new FileInputStream(src));

					if (xsltOutFormat.equals(destExt)) {
						// The XSLT is for transforming to the wanted output, so
						// just execute it
						Result result = new StreamResult(new FileOutputStream(dest));

						// Apply the xsl file to the source file and write the
						// result to the output file
						xformer.transform(source, result);
					} else {
						// The XSLT is for transforming to a different format so
						// execute the transformation and then the conversion
						File transformedFile = null;
						try {
							transformedFile = File.createTempFile("trs", "." + xsltOutFormat);
							Result result = new StreamResult(new FileOutputStream(transformedFile));

							// Apply the xsl file to the source file and write
							// the result to the output file
							xformer.transform(source, result);

							FormatConverter converter = manager.getConverter(xsltOutFormat, destExt);
							if (converter == null)
								throw new IOException(String.format("Unable to find a converter from %s to %s",
										xsltOutFormat, destExt));
							converter.convert(sid, document, transformedFile, dest);
						} finally {
							if (transformedFile != null)
								FileUtil.strongDelete(transformedFile);
						}
					}
				} catch (FileNotFoundException e) {
				} catch (TransformerConfigurationException e) {
					log.warn("An error occurred in the XSL file");
				} catch (TransformerException e) {
					log.warn("An error occurred while applying the XSL file; row:{}, col:{}");
				}
			}
		} catch (Throwable t) {
			if (t instanceof IOException)
				throw (IOException) t;
			else
				throw new IOException(t);
		} finally {
			if (xslt != null)
				FileUtil.strongDelete(xslt);
		}
	}
}