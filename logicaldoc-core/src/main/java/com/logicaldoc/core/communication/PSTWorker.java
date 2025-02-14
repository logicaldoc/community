package com.logicaldoc.core.communication;

import java.io.File;
import java.io.IOException;
import java.text.DateFormat;
import java.text.SimpleDateFormat;
import java.util.Collection;

import org.apache.commons.lang.StringUtils;
import org.jsoup.Jsoup;
import org.slf4j.Logger;
import org.slf4j.LoggerFactory;

import com.pff.PSTException;
import com.pff.PSTFile;
import com.pff.PSTFolder;
import com.pff.PSTMessage;

/**
 * An utility class to work with .pst files. An Outlook Data File (.pst)
 * contains your messages and other Outlook items and is saved on your computer.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.8.5
 */
public class PSTWorker {

	private static final Logger log = LoggerFactory.getLogger(PSTWorker.class);

	private PSTFile pstFile;

	private int depth = -1;

	private long emailsCount;

	private DateFormat df = new SimpleDateFormat("yyyy-MM-dd");

	private StringBuilder buffer = new StringBuilder();

	/**
	 * Constructor
	 * 
	 * @param pstFile the .pst file to open
	 * 
	 * @throws IOException I/O error
	 */
	public PSTWorker(File pstFile) throws IOException {
		try {
			this.pstFile = new PSTFile(pstFile);
		} catch (PSTException e) {
			throw new IOException(e.getMessage(), e);
		}
	}

	/**
	 * Prints the emails of the .pst file
	 * 
	 * @return The listing
	 */
	public String printEmails() {
		buffer = new StringBuilder();
		try {
			printFolderEmails(pstFile.getRootFolder());
			return buffer.toString();
		} catch (PSTException | IOException e) {
			log.error(e.getMessage(), e);
			return "";
		}
	}

	/**
	 * Prints the listing of the .pst file
	 * 
	 * @return The listing
	 */
	public String printListing() {
		depth = -1;
		buffer = new StringBuilder();
		try {
			printFolderListing(pstFile.getRootFolder());
			return buffer.toString();
		} catch (PSTException | IOException e) {
			log.error(e.getMessage(), e);
			return "";
		}
	}

	/**
	 * Cont emails
	 * 
	 * @return the number of contained emails
	 */
	public long countEmails() {
		emailsCount = 0;
		try {
			countFolder(pstFile.getRootFolder());
		} catch (PSTException | IOException e) {
			log.error(e.getMessage(), e);
		}
		return emailsCount;
	}

	private void countFolder(PSTFolder folder) throws PSTException, java.io.IOException {
		// go through the folders...
		if (folder.hasSubfolders()) {
			Collection<PSTFolder> childFolders = folder.getSubFolders();
			for (PSTFolder childFolder : childFolders) {
				countFolder(childFolder);
			}
		}

		// and now the emails for this folder
		if (folder.getContentCount() > 0) {
			PSTMessage email = (PSTMessage) folder.getNextChild();
			while (email != null) {
				emailsCount++;
				email = (PSTMessage) folder.getNextChild();
			}
		}
	}

	private void printFolderListing(PSTFolder folder) throws PSTException, java.io.IOException {
		depth++;

		// the root folder doesn't have a display name
		if (depth > 0) {
			printDepth();
			buffer.append(folder.getDisplayName());
			buffer.append("\n");
		}

		// go through the folders...
		if (folder.hasSubfolders()) {
			Collection<PSTFolder> childFolders = folder.getSubFolders();
			for (PSTFolder childFolder : childFolders) {
				printFolderListing(childFolder);
			}
		}

		// and now the emails for this folder
		if (folder.getContentCount() > 0) {
			depth++;
			PSTMessage email = (PSTMessage) folder.getNextChild();
			while (email != null) {
				printDepth();
				buffer.append(df.format(email.getCreationTime()));
				buffer.append(": ");
				buffer.append(email.getSubject());
				buffer.append("\n");
				email = (PSTMessage) folder.getNextChild();
			}
			depth--;
		}
		depth--;
	}

	private void printDepth() {
		for (int x = 0; x < depth - 1; x++) {
			buffer.append(" | ");
		}
		buffer.append(" |- ");
	}

	private void printFolderEmails(PSTFolder folder) throws PSTException, java.io.IOException {
		// go through the folders...
		if (folder.hasSubfolders()) {
			Collection<PSTFolder> childFolders = folder.getSubFolders();
			for (PSTFolder childFolder : childFolders) {
				printFolderEmails(childFolder);
			}
		}

		// and now the emails for this folder
		if (folder.getContentCount() > 0) {
			PSTMessage email = (PSTMessage) folder.getNextChild();
			while (email != null) {
				printEmail(email);
				email = (PSTMessage) folder.getNextChild();
			}
		}
	}

	private void printEmail(PSTMessage email) throws PSTException, IOException {
		buffer.append("\n");
		buffer.append(df.format(email.getCreationTime()));
		buffer.append(": ");
		buffer.append(email.getSubject());
		buffer.append("\n");
		if (email.getNumberOfRecipients() > 0) {
			buffer.append(email.getRecipientsString());
			buffer.append("\n");
		}
		if (StringUtils.isNotEmpty(email.getBody())) {
			buffer.append(email.getBody());
		} else if (StringUtils.isNotEmpty(email.getBodyHTML())) {
			String html = email.getBodyHTML();
			try {
				org.jsoup.nodes.Document doc = Jsoup.parse(html);
				buffer.append(doc.body().text());
			} catch (Exception e) {
				if (log.isDebugEnabled())
					log.debug("Failed to extract HTML text content", e);
				else
					log.warn("Failed to extract HTML text content");
			}
		}
		buffer.append("\n");
	}
}
