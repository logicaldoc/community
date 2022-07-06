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

/**
 * A converter that simply copies the source to the destination
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.0
 */
public class NoConversionConverter implements FormatConverter {

	protected static Logger log = LoggerFactory.getLogger(NoConversionConverter.class);

	@Override
	public void convert(File src, File dest) throws IOException {
		FileUtil.copyFile(src, dest);
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
	}

	@Override
	public boolean isEnabled() {
		return true;
	}

	@Override
	public void setEnabled(boolean enabled) {

	}
}