package com.logicaldoc.util.io;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.InputStream;
import java.io.OutputStream;

import org.bouncycastle.cms.CMSSignedData;
import org.bouncycastle.util.encoders.Base64;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

/**
 * Use this class to read data from a .p7m file that a signed file
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 4.5
 */
public class P7M {

	protected static Logger log = LoggerFactory.getLogger(P7M.class);

	private File file;

	private CMSSignedData cms;

	private byte[] content;

	public P7M(File file) throws Exception {
		super();
		this.file = file;
	}

	public P7M(InputStream is) throws Exception {
		super();
		read(is);
	}

	public P7M(byte[] content) throws Exception {
		super();
		read(content);
	}

	public void read(byte[] content) {
		this.content = content;

		try {
			this.cms = new CMSSignedData(content);
		} catch (Exception ex1) {
			log.error("Error extracting file certificate");
		}
	}

	/**
	 * Reads a p7m file from a stream. Sets the signed data with the stream as
	 * content.
	 * 
	 * @param is The inputStream
	 */
	public void read(InputStream is) {
		byte[] buffer = new byte[4096];
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
		try {
			while (is.read(buffer) > 0) {
				baos.write(buffer);
			}
		} catch (Exception ex) {
			log.error("Error reading file");
		}
		byte[] tmp = baos.toByteArray();

		try {
			// if the content is on Base64, we must decode it into DER format
			content = Base64.decode(tmp);
			log.debug("Decoding on Base64 completed");
			log.debug("The signed file is in DER format");
		} catch (Exception e) {
			// the content has the DER format
			content = tmp;
			log.debug("The signed file is probably in DER format");
		}

		read(content);
	}

	/**
	 * Reads a p7m file from a file.
	 * 
	 * @throws Exception if the file cannot be read
	 */
	public void read() throws Exception {
		read(new FileInputStream(file));
	}

	/**
	 * Extracts the original file content as stream
	 * 
	 * @return the stream representing the enclosed file
	 * 
	 * @throws Exception in case the enclosed file cannot be extracted
	 */
	public InputStream extractOriginalFileStream() throws Exception {
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		cms.getSignedContent().write(os);
		return new ByteArrayInputStream(os.toByteArray());
	}

	/**
	 * Extracts the original file content into the given file
	 * 
	 * @param outFile The file in which will contained the original file
	 *        content.
	 * 
	 * @throws Exception in case the embedded file cannot be extracted
	 */
	public void extractOriginalFile(File outFile) throws Exception {
		try(OutputStream os = new FileOutputStream(outFile)){
			cms.getSignedContent().write(os);
			os.flush();
		}
	}

	public File getFile() {
		return file;
	}

	public void setFile(File file) {
		this.file = file;
	}

	public CMSSignedData getCms() {
		return cms;
	}
}
