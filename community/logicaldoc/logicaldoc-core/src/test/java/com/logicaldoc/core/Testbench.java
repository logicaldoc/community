package com.logicaldoc.core;

import java.io.File;
import java.net.URL;
import java.util.List;

import org.apache.commons.io.FileUtils;
import org.jdom.Element;
import org.jdom.ProcessingInstruction;
import org.jdom.input.SAXBuilder;

import com.logicaldoc.core.communication.EMailSender;
import com.logicaldoc.util.io.FileUtil;

public class Testbench {

	public static void main(String[] args) throws Exception {
		SAXBuilder saxBuilder = new SAXBuilder();
		org.jdom.Document doc = saxBuilder.build(new File("C:\\workspace\\invoicing\\PA\\IT01879020517_calot.xml"));

		List<Object> contents = doc.getContent();
		for (Object content : contents) {
			if (content instanceof ProcessingInstruction) {
				ProcessingInstruction pi = (ProcessingInstruction) content;
				if ("text/xsl".equals(pi.getPseudoAttributeValue("type"))) {
					File xsltTmp=File.createTempFile("xslt-", ".xsl");
					try{
						FileUtils.copyURLToFile(new URL(pi.getPseudoAttributeValue("href")), xsltTmp);
						org.jdom.Document xsltDoc = saxBuilder.build(xsltTmp);
						Element root = xsltDoc.getRootElement();
						Element outputElem = root.getChild("output",root.getNamespace());
						if(outputElem!=null){
							System.out.println(outputElem.getAttributeValue("method"));
						}
					}finally{
						FileUtil.strongDelete(xsltTmp);
					}
					
//					Scanner scanner = null;
//					try {
//						scanner = new Scanner(new URL(pi.getPseudoAttributeValue("href")).openStream(),
//								StandardCharsets.UTF_8.toString());
//						scanner.useDelimiter("\\A");
//						String styleContent = scanner.hasNext() ? scanner.next() : "";
//						System.out.println(styleContent);
//						
//						org.jdom.Document doc = saxBuilder.build(new File("C:\\workspace\\invoicing\\PA\\IT01879020517_calot.xml"));10
//					} finally {
//						if (scanner != null)
//							scanner.close();
//					}
				}
			}
		}

		// try {
		// // Create transformer factory
		// TransformerFactory factory = TransformerFactory.newInstance();
		//
		// // Use the factory to create a template containing the xsl file
		// Templates template = factory.newTemplates(new StreamSource(
		// new
		// FileInputStream("C:\\workspace\\invoicing\\PA\\fatturaordinaria_v1.2.1.xsl")));
		//
		// // Use the template to create a transformer
		// Transformer xformer = template.newTransformer();
		//
		// // Prepare the input and output files
		// Source source = new StreamSource(new
		// FileInputStream("C:\\workspace\\invoicing\\PA\\IT01879020517_calot.xml"));
		// Result result = new StreamResult(new
		// FileOutputStream("C:\\tmp\\pa.html"));
		//
		// // Apply the xsl file to the source file and write the result
		// // to the output file
		// xformer.transform(source, result);
		// } catch (FileNotFoundException e) {
		// } catch (TransformerConfigurationException e) {
		// // An error occurred in the XSL file
		// } catch (TransformerException e) {
		// // An error occurred while applying the XSL file
		// // Get location of error in input file
		// SourceLocator locator = e.getLocator();
		// int col = locator.getColumnNumber();
		// int line = locator.getLineNumber();
		// String publicId = locator.getPublicId();
		// String systemId = locator.getSystemId();
		// }

		// Automation engine = new Automation(null, null, 1L);

		// String original="1234567€";
		// System.out.println("original: "+original);
		//
		// String encoded=URLEncoder.encode(original, "UTF-8");
		// System.out.println("encoded: "+encoded);
		//
		//
		// String decoded=URLDecoder.decode("1234567%E2%82%AC", "UTF-8");
		// System.out.println("decoded: "+decoded);

		// EMailSender sender = prepareEmailSender();
		//
		// EMail eml = new EMail();
		// eml.setSubject("HTML test");
		// eml.setHtml(1);
		// Set<Recipient> recipients = new HashSet<Recipient>();
		//
		// eml.getImages().add("file:///C:/Users/Marco/Pictures/email_header2.jpg");
		// //"http://www.logicaldoc.com/templates/theme326/images/LogicalDOC_logo_website.gif");
		//
		// // Notify the general report
		// Recipient rec = new Recipient();
		// rec.setAddress("a.gasparini@logicalobjects.com");
		// recipients.add(rec);
		// eml.setRecipients(recipients);
		// //
		// eml.setMessageText("<span style='color:red'>Test</span> di <b>HTML</b>\ncippa"
		// // +"<br/><image src='cid:image_1' />");
		//
		// String message = StringUtil.writeToString(new FileInputStream(new
		// File("C:/tmp/ldoc_ready.html")), "UTF8");
		// message=message.replaceAll("\\{", "'{'");
		// message=message.replaceAll("}", "'}'");
		// message=message.replaceAll("\\[\\[", "{");
		// message=message.replaceAll("\\]\\]", "}");
		//
		//
		// MessageFormat format = new MessageFormat(message);
		// message = format.format(new Object[] { "Marco Meschieri",
		// "qui c'è il codice di attivazione" });
		// eml.setMessageText(message);
		//
		// System.out.println(eml.getMessageText());
		//
		// sender.send(eml);
	}

	protected static EMailSender prepareEmailSender() {
		EMailSender sender = new EMailSender();
		sender.setHost("smtp.logicalobjects.com");
		sender.setPort(25);
		sender.setUsername("m.meschieri@logicalobjects.com");
		sender.setPassword("paf75peR");
		sender.setAuthEncripted(false);
		sender.setConnectionSecurity(0);
		sender.setSender("m.meschieri@logicalobjects.com");
		return sender;
	}
}
