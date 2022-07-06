package com.logicaldoc.webdav.context;

import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.util.Date;

import org.apache.jackrabbit.server.io.IOListener;
import org.apache.jackrabbit.server.io.IOUtil;
import org.apache.jackrabbit.webdav.io.InputContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.webdav.resource.model.Resource;

/**
 * For more informations, please visit
 * {@link org.apache.jackrabbit.server.io.ImportContextImpl}
 * 
 * @author Sebastian Wenzky
 * 
 */
public class ImportContextImpl implements ImportContext {

	protected static Logger log = LoggerFactory.getLogger(ImportContextImpl.class);

	private final Resource resource;

	private String systemId;

	private File inputFile;

	private InputContext inputCtx;

	private boolean completed;

	public ImportContextImpl(Resource resource, String systemId, InputContext inputCtx) throws IOException {
		this(resource, systemId, (inputCtx != null) ? inputCtx.getInputStream() : null);
		this.inputCtx = inputCtx;
	}

	public ImportContextImpl(Resource resource, String systemId, InputStream in) throws IOException {
		this.resource = resource;
		this.systemId = systemId;
		this.inputFile = IOUtil.getTempFile(in);
	}

	public IOListener getIOListener() {
		return null;
	}

	public boolean hasStream() {
		return inputFile != null;
	}

	public InputStream getInputStream() {
		checkCompleted();
		InputStream in = null;
		if (inputFile != null) {
			try {
				in = new FileInputStream(inputFile);
			} catch (IOException e) {
				// unexpected error... ignore and return null
			}
		}
		return in;
	}

	public String getSystemId() {
		return systemId;
	}

	public long getModificationTime() {
		return (inputCtx != null) ? inputCtx.getModificationTime() : new Date().getTime();
	}

	public String getContentLanguage() {
		return (inputCtx != null) ? inputCtx.getContentLanguage() : null;
	}

	public long getContentLength() {
		if (inputCtx != null) {
			return inputCtx.getContentLength();
		} else if (inputFile != null) {
			return inputFile.length();
		} else {
			log.debug("Unable to determine content length -> default value = " + IOUtil.UNDEFINED_LENGTH);
			return IOUtil.UNDEFINED_LENGTH;
		}
	}

	private String getContentType() {
		return (inputCtx != null) ? inputCtx.getContentType() : null;
	}

	public String getMimeType() {
		String contentType = getContentType();
		return IOUtil.getMimeType(contentType);
	}

	public String getEncoding() {
		String contentType = getContentType();
		return (contentType != null) ? IOUtil.getEncoding(contentType) : null;
	}

	public Object getProperty(Object propertyName) {
		return (inputCtx != null) ? inputCtx.getProperty(propertyName.toString()) : null;
	}

	public void informCompleted(boolean success) {
		checkCompleted();
		completed = true;
		if (inputFile != null) {
			inputFile.delete();
		}
	}

	public boolean isCompleted() {
		return completed;
	}

	private void checkCompleted() {
		if (completed) {
			throw new IllegalStateException("ImportContext has already been consumed.");
		}
	}

	public Resource getResource() {
		return this.resource;
	}

	public void setSystemId(String newResourceName) {
		this.systemId = newResourceName;
	}
	
	public void setInputFile(File inputFile) {
		this.inputFile = inputFile;
	}	
}
