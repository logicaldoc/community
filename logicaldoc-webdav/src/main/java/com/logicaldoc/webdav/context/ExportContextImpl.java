package com.logicaldoc.webdav.context;

import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.OutputStream;
import java.util.Date;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;

import org.apache.jackrabbit.server.io.IOUtil;
import org.apache.jackrabbit.webdav.DavConstants;
import org.apache.jackrabbit.webdav.io.OutputContext;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.util.io.FileUtil;
import com.logicaldoc.webdav.resource.model.Resource;

/**
 * For more informations, please visit
 * {@link org.apache.jackrabbit.server.io.ExportContextImpl}
 * 
 * @author Sebastian Wenzky
 * 
 */
public class ExportContextImpl extends AbstractExportContext {

	protected static Logger log = LoggerFactory.getLogger(ExportContextImpl.class);

	@SuppressWarnings("rawtypes")
	private final Map properties = new HashMap();

	private final OutputContext outputCtx;

	private File outFile;

	private OutputStream outStream;

	public ExportContextImpl(Resource resource, OutputContext outputCtx) throws IOException {
		super(resource, (outputCtx != null) && outputCtx.hasStream());
		this.outputCtx = outputCtx;
		if (hasStream()) {
			// we need a tmp file, since the export could fail
			outFile = FileUtil.createTempFile("__exportcontext", "tmp");
		}
	}

	public OutputStream getOutputStream() {
		checkCompleted();
		if (hasStream()) {
			try {
				// clean up the stream retrieved by the preceeding handler, that
				// did not behave properly and failed to export although
				// initially willing to handle the export.
				if (outStream != null) {
					outStream.close();
				}
				outStream = new FileOutputStream(outFile);
				return outStream;
			} catch (IOException e) {
				// unexpected error... ignore and return null
			}
		}
		return null;
	}

	@SuppressWarnings("unchecked")
	public void setContentLanguage(String contentLanguage) {
		properties.put(DavConstants.HEADER_CONTENT_LANGUAGE, contentLanguage);
	}

	@SuppressWarnings("unchecked")
	public void setContentLength(long contentLength) {
		properties.put(DavConstants.HEADER_CONTENT_LENGTH, contentLength + "");
	}

	@SuppressWarnings("unchecked")
	public void setContentType(String mimeType, String encoding) {
		properties.put(DavConstants.HEADER_CONTENT_TYPE, IOUtil.buildContentType(mimeType, encoding));
	}

	public void setCreationTime(long creationTime) {
		// ignore since output-ctx does not understand creation time
	}

	@SuppressWarnings("unchecked")
	public void setModificationTime(long modificationTime) {
		if (modificationTime <= IOUtil.UNDEFINED_TIME) {
			modificationTime = new Date().getTime();
		}
		String lastMod = IOUtil.getLastModified(modificationTime);
		properties.put(DavConstants.HEADER_LAST_MODIFIED, lastMod);
	}

	@SuppressWarnings("unchecked")
	public void setETag(String etag) {
		properties.put(DavConstants.HEADER_ETAG, etag);
	}

	@SuppressWarnings("unchecked")
	public void setProperty(Object propertyName, Object propertyValue) {
		properties.put(propertyName, propertyValue);
	}

	@SuppressWarnings("rawtypes")
	@Override
	public void informCompleted(boolean success) {
		checkCompleted();
		completed = true;
		// make sure the outputStream gets closed (and don't assume the handlers
		// took care of this.
		if (outStream != null) {
			try {
				outStream.close();
			} catch (IOException e) {
				// ignore
			}
		}

		// write properties and data to the output-context
		if (success && outputCtx != null) {
			boolean hasContentLength = false;
			Iterator it = properties.keySet().iterator();
			while (it.hasNext()) {
				Object name = it.next();
				Object value = properties.get(name);
				if (name != null && value != null) {
					outputCtx.setProperty(name.toString(), value.toString());
					// check for content-length
					hasContentLength = DavConstants.HEADER_CONTENT_LENGTH.equals(name.toString());
				}
			}

			writeFile(hasContentLength);
		}

		FileUtil.strongDelete(outFile);
	}

	private void writeFile(boolean hasContentLength) {
		if (outputCtx.hasStream() && outFile != null) {
			OutputStream out = outputCtx.getOutputStream();
			try {
				// make sure the content-length is set
				if (!hasContentLength) {
					outputCtx.setContentLength(outFile.length());
				}
				FileInputStream in = new FileInputStream(outFile);
				IOUtil.spool(in, out);
			} catch (IOException e) {
				log.error(e.toString());
			}
		}
	}
}
