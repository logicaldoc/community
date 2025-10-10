package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.IOException;

import org.commonmark.node.Node;
import org.commonmark.parser.Parser;
import org.commonmark.renderer.html.HtmlRenderer;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.spring.Context;

/**
 * Converter to convert Markdown files to PDF or HTML
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.7
 */
public class MarkdownConverter extends AbstractFormatConverter {

	@Override
	public void internalConvert(String sid, Document document, File src, File dest) throws IOException {
		File htmlTemp = null;
		File pdfTemp = null;
		try {
			htmlTemp = FileUtil.createTempFile("mkconvert", ".html");
			Parser parser = Parser.builder().build();
			Node mkDocument = parser.parse(FileUtil.readFile(src));
			HtmlRenderer renderer = HtmlRenderer.builder().build();
			FileUtil.writeFile(renderer.render(mkDocument), htmlTemp.getAbsolutePath());

			if (dest.getName().toLowerCase().endsWith(".pdf")) {
				pdfTemp = FileUtil.createTempFile("mkconvert", ".pdf");
				FormatConverterManager manager = Context.get(FormatConverterManager.class);
				manager.convertFile(htmlTemp, "markdown.html", pdfTemp, "pdf", sid);
				FileUtil.copyFile(pdfTemp, dest);
			} else {
				FileUtil.copyFile(htmlTemp, dest);
			}
		} catch (Exception e) {
			throw new IOException(e.getMessage(), e);
		} finally {
			FileUtil.delete(htmlTemp);
			FileUtil.delete(pdfTemp);
		}
	}
}