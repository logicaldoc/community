package com.logicaldoc.core.parser;

import java.io.BufferedInputStream;
import java.io.BufferedReader;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.OutputStream;
import java.io.StringReader;
import java.util.StringTokenizer;
import java.util.regex.Matcher;
import java.util.regex.Pattern;

import javax.swing.text.BadLocationException;
import javax.swing.text.DefaultStyledDocument;
import javax.swing.text.rtf.RTFEditorKit;

import org.apache.commons.lang.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.StringUtil;
import com.logicaldoc.util.io.FileUtil;

/**
 * A parser for the Rich Text Format
 * 
 * @author Michael Scholz
 * @author Alessandro Gasparini
 * @since 3.5
 */
public class RTFParser extends AbstractParser {

	protected static Logger log = LoggerFactory.getLogger(RTFParser.class);

	@Override
	public void internalParse(InputStream input, ParseParameters parameters, StringBuilder content) {

		try (BufferedInputStream bis0 = new BufferedInputStream(input);) {
			bis0.mark(Integer.MAX_VALUE);

			String text = extractText(bis0);
			content.append(StringUtil.writeToString(new StringReader(text)));

			// Check if there are some variable code that must be added to the
			// content
			bis0.reset();
			File tempFile = FileUtil.createTempFile("rtf", ".rtf");
			try (OutputStream out = new FileOutputStream(tempFile)) {
				byte[] buf = new byte[512];
				int len;
				while ((len = bis0.read(buf)) > 0)
					out.write(buf, 0, len);
			}

			StringBuilder strBuf = null;
			try (FileInputStream fis = new FileInputStream(tempFile);
					BufferedReader d = new BufferedReader(new InputStreamReader(fis))) {
				String thisLine = "";

				strBuf = new StringBuilder();
				strBuf.append(content);

				Pattern pattern = Pattern.compile("fldinst MERGEFIELD ");
				StringTokenizer st = null;
				while ((thisLine = d.readLine()) != null) {
					for (String string : pattern.split(thisLine)) {
						string = string.replace("\\fldrslt", "");
						string = string.replace("\\{", "");
						st = new StringTokenizer(string, "}");
						while (st.hasMoreTokens()) {
							String token = st.nextToken();
							if (StringUtils.isNotEmpty(token) && !token.startsWith("\\")) {
								strBuf.append(token);
							}
						}
						strBuf.append("\n");
					}
				}
			}

			input.close();

			content.append(strBuf.toString());

			// Delete temp file when program exits.
			tempFile.deleteOnExit();

		} catch (Exception t) {
			log.warn("Failed to extract RTF text content", t);
		}
	}

	protected String extractText(InputStream input) throws IOException, BadLocationException {
		RTFEditorKit rek = new RTFEditorKit();
		DefaultStyledDocument doc = new DefaultStyledDocument();
		rek.read(input, doc, 0);
		return doc.getText(0, doc.getLength());
	}

	@Override
	public int countPages(InputStream input, String filename) {
		try {
			String text = StringUtil.writeToString(input, "UTF-8");
			if (text == null)
				return 1;

			Pattern pattern = Pattern.compile("\\\\nofpages\\d+");
			Matcher matcher = pattern.matcher(text);
			if (matcher.find()) {
				String nofpages = matcher.group(0);
				String npages = nofpages.substring(9);
				return Integer.parseInt(npages);
			}
		} catch (Exception e) {
			log.error(e.getMessage());
		}
		return 1;
	}

	@Override
	public int countPages(File input, String filename) {

		try (FileInputStream stream = new FileInputStream(input)) {
			return this.countPages(stream, filename);
		} catch (IOException e) {
			log.error(e.getMessage());
		}
		return 1;
	}
}