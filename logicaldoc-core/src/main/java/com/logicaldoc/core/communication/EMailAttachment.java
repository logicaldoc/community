package com.logicaldoc.core.communication;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.Serializable;
import java.util.Locale;

import org.apache.commons.lang3.StringUtils;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.logicaldoc.core.parser.Parser;
import com.logicaldoc.core.parser.ParserFactory;
import com.logicaldoc.core.security.Tenant;
import com.logicaldoc.core.security.TenantDAO;
import com.logicaldoc.util.spring.Context;

/**
 * An attachment of an email message
 * 
 * @author Michael Scholz
 * @author Alessandro Gasparini - LogicalDOC
 */
public class EMailAttachment implements Serializable {

	private static final long serialVersionUID = 1L;

	private static final Logger log = LoggerFactory.getLogger(EMailAttachment.class);

	private String icon = "";

	private byte[] data;

	private long size;

	private String mimeType = "";
	
	/**
	 * Should be used to fill the Content-Disposition header
	 */
	private String disposition;

	/**
	 * Should be used to fill the Content-Type header
	 */
	private String contentType;
	
	/**
	 * Should be used to fill the Content-Transfer-Encoding header
	 */
	private String contentEncoding;
	
	private String fileName = "";

	public String getIcon() {
		return icon;
	}

	public void setIcon(String icon) {
		this.icon = icon;
	}

	public String getMimeType() {
		return mimeType;
	}

	public void setMimeType(String string) {
		mimeType = string;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public byte[] getData() {
		return data;
	}

	public void setData(byte[] data) {
		this.data = data;
	}

	public long getSize() {
		return size;
	}

	public void setSize(long size) {
		this.size = size;
	}

	/**
	 * Parse the attachment extracting the text using the parser configured for
	 * this specific filename
	 * 
	 * @see EMailAttachment#parseContent(Long, Locale, String)
	 * 
	 * @return the extracted text
	 */
	public String parseContent() {
		return parseContent(null, null, null);
	}

	/**
	 * Parse the attachment extracting the text using the parser configured for
	 * this specific filename
	 * 
	 * @param tenantId tenant specification(optional, default is
	 *        {@link Tenant#DEFAULT_ID })
	 * @param locale the language in which the attachment is written(optional,
	 *        defaults to english)
	 * @param encoding the character encoding(optional, defaults to UTF-8)
	 * 
	 * @return the extracted text
	 */
	public String parseContent(Long tenantId, Locale locale, String encoding) {
		String content = null;

		Parser parser = ParserFactory.getParser(getFileName());
		if (parser == null) {
			log.debug("Unexisting parser for attachment {}", getFileName());
			return content;
		}

		log.debug("Using parser {} to parse attachment {}", parser.getClass().getName(), getFileName());

		// and gets some fields
		try (InputStream contentStream = new ByteArrayInputStream(getData())) {
			if (tenantId != null) {
				TenantDAO tDao = Context.get(TenantDAO.class);
				content = parser.parse(contentStream, getFileName(),
						StringUtils.isNotEmpty(encoding) ? encoding : "UTF-8", locale != null ? locale : Locale.ENGLISH,
						tDao.findById(tenantId).getName());
			} else
				content = parser.parse(contentStream, getFileName(),
						StringUtils.isNotEmpty(encoding) ? encoding : "UTF-8", locale != null ? locale : Locale.ENGLISH,
						Tenant.DEFAULT_NAME);
		} catch (Exception e) {
			log.error(e.getMessage(), e);
		}

		return content;
	}

	public String getDisposition() {
		return disposition;
	}

	public void setDisposition(String disposition) {
		this.disposition = disposition;
	}

	public String getContentType() {
		return contentType;
	}

	public void setContentType(String contentType) {
		this.contentType = contentType;
	}

	public String getContentEncoding() {
		return contentEncoding;
	}

	public void setContentEncoding(String contentEncoding) {
		this.contentEncoding = contentEncoding;
	}
}
