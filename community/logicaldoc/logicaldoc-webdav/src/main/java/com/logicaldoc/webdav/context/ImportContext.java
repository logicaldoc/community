package com.logicaldoc.webdav.context;

import java.io.InputStream;

import org.apache.jackrabbit.server.io.IOContext;

import com.logicaldoc.webdav.resource.model.Resource;

/**
 * For more informations, please visit
 * {@link org.apache.jackrabbit.server.io.ImportContext}
 * 
 * @author Sebastian Wenzky
 * 
 */
public interface ImportContext extends IOContext {

	public Resource getResource();

	public String getSystemId();

	public InputStream getInputStream();

	public long getModificationTime();

	public String getContentLanguage();

	public long getContentLength();

	public String getMimeType();

	public String getEncoding();

	public Object getProperty(Object propertyName);
}
