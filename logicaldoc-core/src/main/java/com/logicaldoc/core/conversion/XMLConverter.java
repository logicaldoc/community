package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.net.URL;
import java.util.Arrays;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

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
import org.apache.commons.lang3.StringUtils;
import org.jdom2.Content;
import org.jdom2.Element;
import org.jdom2.ProcessingInstruction;
import org.jdom2.input.SAXBuilder;
import org.jdom2.input.sax.XMLReaders;
import org.jsoup.Jsoup;
import org.jsoup.select.Elements;
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

	private static final String STYLE_SHEETS = "styleSheets";

	protected static Logger log = LoggerFactory.getLogger(XMLConverter.class);

	@Override
	public void internalConvert(String sid, Document document, File src, File dest) throws IOException {
		String destExt = FileUtil.getExtension(dest.getName()).toLowerCase();
		File xslt = null;
		File xml = null;
		String xsltOutFormat = null;
		try {
			xslt = File.createTempFile("xslt", ".xsl");
			xml = File.createTempFile("xml", ".xml");
			FileUtil.copyFile(src, xml);

			// Parse the XML searching for a stylesheet
			SAXBuilder builder = new SAXBuilder(XMLReaders.NONVALIDATING);

			builder.setFeature("http://xml.org/sax/features/validation", false);
			builder.setFeature("http://apache.org/xml/features/nonvalidating/load-dtd-grammar", false);
			builder.setFeature("http://apache.org/xml/features/nonvalidating/load-external-dtd", false);
			builder.setFeature("http://xml.org/sax/features/external-general-entities", false);
			builder.setFeature("http://apache.org/xml/features/disallow-doctype-decl", true);
			builder.setFeature("http://xml.org/sax/features/external-parameter-entities", false);
			
//			builder.setProperty(XMLConstants.ACCESS_EXTERNAL_DTD, "");
//			builder.setProperty(XMLConstants.ACCESS_EXTERNAL_SCHEMA, "");

			// builder.setExpandEntities(false);

			/*
			 * SAXBuilder builder = new SAXBuilder(); builder.setFeature(
			 * "http://apache.org/xml/features/disallow-doctype-decl",true);
			 * builder.setFeature(
			 * "http://xml.org/sax/features/external-general-entities", false);
			 * builder.setFeature(
			 * "http://xml.org/sax/features/external-parameter-entities",
			 * false); builder.setExpandEntities(false);
			 */

			org.jdom2.Document doc = builder.build(xml);
			List<Content> contents = doc.getContent();

			boolean containsStyleReference = false;
			for (Object content : contents) {
				if (content instanceof ProcessingInstruction) {
					ProcessingInstruction pi = (ProcessingInstruction) content;
					if ("text/xsl".equals(pi.getPseudoAttributeValue("type"))) {
						containsStyleReference = true;

						// Found a style sheet, download it and check the output
						// format
						try {
							FileUtils.copyURLToFile(new URL(pi.getPseudoAttributeValue("href")), xslt);
							org.jdom2.Document xsltDoc = builder.build(xslt);
							Element root = xsltDoc.getRootElement();
							Element outputElem = root.getChild("output", root.getNamespace());
							if (outputElem != null)
								xsltOutFormat = outputElem.getAttributeValue("method").toLowerCase();
						} catch (FileNotFoundException e) {
							log.warn("Cannot find the referenced style sheet {}", pi.getPseudoAttributeValue("href"));
						}
					}
				}
			}

			String rootElmentName = doc.getRootElement().getName();
			String style = getStyleByRootElement(rootElmentName);

			if (StringUtils.isNotEmpty(style)) {
				log.debug("Force the stylesheed {} for converting xml file {}", style, document.getFileName());

				try {
					FileUtils.copyURLToFile(new URL(style), xslt);
					xsltOutFormat = "html";
					String newStyleSpec = "<?xml-stylesheet type=\"text/xsl\" href=\"" + style + "\" ?>";
					if (containsStyleReference) {
						FileUtil.replaceInFile(xml.getAbsolutePath(), "<\\?xml-stylesheet type=\\\"text/xsl\\\".*\\?>",
								newStyleSpec);
					} else {
						FileUtil.replaceInFile(xml.getAbsolutePath(), "<" + doc.getRootElement().getQualifiedName(),
								"\n" + newStyleSpec + "\n<" + doc.getRootElement().getQualifiedName());
					}
				} catch (FileNotFoundException e) {
					log.warn("Cannot find the referenced style sheet {}", style);
				} catch (Throwable t) {
					log.warn("Cannot elaborate the style sheet {}", style, t);
				}
			}

			FormatConverterManager manager = (FormatConverterManager) Context.get()
					.getBean(FormatConverterManager.class);
			if (xsltOutFormat == null) {
				FormatConverter converter = manager.getConverter("txt", destExt);
				if (converter == null)
					throw new IOException(String.format("Unable to find a converter from txt to %s", destExt));
				converter.convert(sid, document, xml, dest);
			} else {
				try {
					// Create transformer factory
					TransformerFactory factory = TransformerFactory.newInstance();

					// Use the factory to create a template containing the
					// xsl file
					Templates template = factory.newTemplates(new StreamSource(new FileInputStream(xslt)));

					// Use the template to create a transformer
					Transformer xformer = template.newTransformer();

					// Prepare the input file
					Source source = new StreamSource(new FileInputStream(xml));

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

							if ("html".equals(xsltOutFormat.toLowerCase())) {
								// Sometimes the converter adds a not closed
								// element <META http-equiv="Content-Type"
								// content="text/html; charset=UTF-8">, so we
								// must strip not closed <meta> tags
								String htmlContent = FileUtil.readFile(transformedFile);
								org.jsoup.nodes.Document htmlDoc = Jsoup.parse(htmlContent);
								Elements elements = htmlDoc.select("meta");
								for (org.jsoup.nodes.Element element : elements) {
									if (!element.toString().endsWith("/>"))
										element.remove();
								}
								htmlContent = htmlDoc.html();
								FileUtil.writeFile(htmlContent, transformedFile.getAbsolutePath());
							}

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
					log.warn("File not found", e);
				} catch (TransformerConfigurationException e) {
					log.warn("An error occurred in the XSL file", e);
				} catch (TransformerException e) {
					log.warn("An error occurred while applying the XSL file; row:{}, col:{}", e);
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
			if (xml != null)
				FileUtil.strongDelete(xml);
		}
	}

	private String getStyleByRootElement(String rootElementName) {
		Map<String, String> styles = loadStyleSheets();
		return styles.get(rootElementName);
	}

	private Map<String, String> loadStyleSheets() {
		Map<String, String> map = new HashMap<String, String>();
		String value = getParameter(STYLE_SHEETS);
		if (StringUtils.isNotEmpty(value)) {
			String[] styles = value.split("\\,");
			for (String style : styles) {
				if (StringUtils.isNoneEmpty(style)) {
					if (style.contains("|")) {
						String[] tokens = style.trim().split("\\|");
						map.put(tokens[0].trim(), tokens[1].trim());
					}
				}
			}
		}
		return map;
	}

	@Override
	public List<String> getParameterNames() {
		return Arrays.asList(new String[] { STYLE_SHEETS });
	}
}