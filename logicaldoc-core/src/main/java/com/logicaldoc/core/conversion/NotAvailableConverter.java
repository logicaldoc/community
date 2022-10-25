package com.logicaldoc.core.conversion;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.document.Document;
import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.util.io.ResourceUtil;

/**
 * A converter that always convert into a static PDF that says 'not available'
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.4
 */
public class NotAvailableConverter implements FormatConverter {

	protected static Logger log = LoggerFactory.getLogger(NotAvailableConverter.class);

	@Override
	public void convert(File src, File dest) throws IOException {
		String ext = FileUtil.getExtension(dest.getName());

		if (!"pdf".equalsIgnoreCase(ext))
			throw new IOException(String.format("Format %s not supported", ext));

		ResourceUtil.copyResource("/pdf/notavailable.pdf", dest);
	}

	@Override
	public void convert(String sid, Document document, File src, File dest) throws IOException {
		convert(src, dest);
	}

	@Override
	public List<String> getParameterNames() {
		return new ArrayList<String>();
	}

	@Override
	public Map<String, String> getParameters() {
		return new HashMap<String, String>();
	}

	@Override
	public String getParameter(String name) {
		return null;
	}

	@Override
	public void loadParameters() {
		// Nothing to do
	}

	@Override
	public boolean isEnabled() {
		return true;
	}

	@Override
	public void setEnabled(boolean enabled) {
		// Nothing to do
	}
}