package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import org.apache.commons.lang.StringUtils;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.exec.Exec;
import com.logicaldoc.util.io.FileUtil;

/**
 * Converter to convert PDF into image
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.5.1
 */
public class GhostscriptConverter extends AbstractFormatConverter {

	@Override
	public void internalConvert(String sid, Document document, File src, File dest) throws IOException {
		String ext = FileUtil.getExtension(dest.getName()).toLowerCase();

		try {
			String arguments = getParameter("arguments");
			List<String> commandLine = new ArrayList<>();
			commandLine.add(getParameter("path"));
			if (StringUtils.isNotBlank(arguments))
				commandLine.addAll(Arrays.asList(arguments.split(" ")));

			commandLine.add("-dNOPAUSE");
			commandLine.add("-dBATCH");

			List<String> pages = null;
			String device = "jpeg";
			if ("tif".equals(ext) || "tiff".equals(ext)) {
				device = "tiff24nc";
				commandLine.add("-sCompression=lzw");
			} else if ("png".equals(ext)) {
				device = "png16m";
				pages = List.of("-dFirstPage=1", "-dLastPage=1");
			} else if ("ps".equals(ext)) {
				device = "ps2write";
				pages = List.of("-dFirstPage=1", "-dLastPage=1");
			} else if ("txt".equals(ext)) {
				device = "txtwrite";
			} else {
				device = "jpeg";
				pages = List.of("-dFirstPage=1", "-dLastPage=1");
			}

			commandLine.add("-sDEVICE=" + device);
			if (pages != null)
				commandLine.addAll(pages);
			commandLine.addAll(List.of("-sOutputFile=" + dest.getPath(), src.getPath()));

			new Exec().exec(commandLine, null, new File("C:\\LogicalDOC-Devel\\ghostscript\\bin"), getTimeout());

			if (!dest.exists() || dest.length() < 1)
				throw new IOException("Empty conversion");
		} catch (IOException ioe) {
			throw ioe;
		} catch (Exception e) {
			throw new IOException("Error in PDF to image conversion", e);
		}
	}

	private int getTimeout() {
		int timeout = 30;
		try {
			timeout = Integer.parseInt(getParameter("timeout"));
		} catch (Exception t) {
			// Nothing to do
		}
		return timeout;
	}

	@Override
	public List<String> getParameterNames() {
		return Arrays.asList("path", "arguments", "timeout");
	}
}