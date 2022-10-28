package com.logicaldoc.util.io;

import java.io.ByteArrayInputStream;
import java.io.ByteArrayOutputStream;
import java.io.File;
import java.io.FileInputStream;
import java.io.FileNotFoundException;
import java.io.FileOutputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.OutputStream;

import org.bouncycastle.cms.CMSException;
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

	public P7M(File file) {
		super();
		this.file = file;
	}

	public P7M(InputStream is) throws IOException {
		super();
		read(is);
	}

	public P7M(byte[] content) {
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
	 * 
	 * @throws IOException I/O error 
	 */
	public void read(InputStream is) throws IOException {
		byte[] buffer = new byte[4096];
		ByteArrayOutputStream baos = new ByteArrayOutputStream();
			while (is.read(buffer) > 0) {
				baos.write(buffer);
			}

		byte[] tmp = baos.toByteArray();


			// if the content is on Base64, we must decode it into DER format
			content = Base64.decode(tmp);
			log.debug("Decoding on Base64 completed");
			log.debug("The signed file is in DER format");
		

		read(content);
	}

	/**
	 * Reads a p7m file from a file.
	 * 
	 * @throws IOException I/O error
	 */
	public void read() throws IOException {
		read(new FileInputStream(file));
	}

	/**
	 * Extracts the original file content as stream
	 * 
	 * @return the stream representing the enclosed file
	 * 
	 * @throws CMSException in case the enclosed file cannot be extracted
	 * @throws IOException I/O error
	 */
	public InputStream extractOriginalFileStream() throws IOException, CMSException {
		ByteArrayOutputStream os = new ByteArrayOutputStream();
		cms.getSignedContent().write(os);
		return new ByteArrayInputStream(os.toByteArray());
	}

	/**
	 * Extracts the original file content into the given file
	 * 
	 * @param outFile The file in which will contained the original file
	 *        content.
	 * @throws IOException I/O exception
	 * @throws FileNotFoundException file not found
	 * @throws CMSException in case the embedded file cannot be extracted
	 */
	public void extractOriginalFile(File outFile) throws FileNotFoundException, IOException, CMSException {
		try (OutputStream os = new FileOutputStream(outFile)) {
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
