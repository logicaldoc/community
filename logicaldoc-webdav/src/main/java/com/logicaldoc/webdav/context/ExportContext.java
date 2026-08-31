package com.logicaldoc.webdav.context;

import java.io.OutputStream;

import org.apache.jackrabbit.server.io.IOContext;

import com.logicaldoc.webdav.resource.model.Resource;

/**
 * For more informations, please visit
 * {@link org.apache.jackrabbit.server.io.ExportContext}
 * 
 * @author Sebastian Wenzky
 * 
 */
public interface ExportContext extends IOContext {

	public Resource getResource();

	public OutputStream getOutputStream();

	public void setContentType(String mimeType, String encoding);

	public void setContentLanguage(String contentLanguage);

	public void setContentLength(long contentLength);

	public void setCreationTime(long creationTime);

	public void setModificationTime(long modificationTime);

	public void setETag(String etag);

	public void setProperty(Object propertyName, Object propertyValue);
}
