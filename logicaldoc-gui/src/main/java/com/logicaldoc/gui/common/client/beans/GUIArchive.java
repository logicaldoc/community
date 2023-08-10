package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.Date;

/**
 * GUI representation of an import/export archive
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class GUIArchive implements Serializable {
	private static final long serialVersionUID = 1L;

	private long id = 0;

	public static final int STATUS_OPENED = 0;

	public static final int STATUS_READY_TO_IMPORT = 0;

	public static final int STATUS_CLOSED = 1;

	public static final int STATUS_FINALIZED = 2;

	public static final int STATUS_READYTOSIGN = 3;

	public static final int STATUS_ERROR = 4;

	public static final int TYPE_DEFAULT = 0;

	public static final int TYPE_STORAGE = 1;

	public static final int MODE_IMPORT = 0;

	public static final int MODE_EXPORT = 1;

	public static final int CUSTOMID_NOT_IMPORT = 0;

	public static final int CUSTOMID_IMPORT_AND_NEW_RELEASE = 1;

	public static final int CUSTOMID_IMPORT_AND_NEW_SUBVERSION = 2;

	public static final int CUSTOMID_IMPORT_AND_NEW_DOCUMENT = 3;

	private String name = "";

	private String description = "";

	private Date creation = new Date();

	private long size = 0;

	private long creatorId;

	private String creatorName = "";

	private Long closerId;

	private String closerName = "";

	private Date closure;

	private int status = STATUS_OPENED;

	private int type = TYPE_DEFAULT;

	private int mode = MODE_IMPORT;

	private int importTemplate = 1;

	private int importCustomId = 0;

	/**
	 * The archive name. Also alternative identifier.
	 * 
	 * @return the name
	 */
	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	/**
	 * The archive description
	 * 
	 * @return description of the archive
	 */
	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	/**
	 * The archive's creation date
	 * 
	 * @return the date when the archive was created
	 */
	public Date getCreation() {
		return creation;
	}

	public void setCreation(Date creation) {
		this.creation = creation;
	}

	/**
	 * The archive status
	 * 
	 * @see #STATUS_OPENED
	 * @see #STATUS_CLOSED
	 * @see #STATUS_FINALIZED
	 * @see #STATUS_READYTOSIGN
	 * @see #STATUS_ERROR
	 * 
	 * @return the status
	 */
	public int getStatus() {
		return status;
	}

	public void setStatus(int status) {
		this.status = status;
	}

	/**
	 * Archive dimension in bytes, that is the sum of sizes of all document plus
	 * the size of all metadata files
	 * 
	 * @return the total size in bytes
	 */
	public long getSize() {
		return size;
	}

	public void setSize(long size) {
		this.size = size;
	}

	/**
	 * The archive type
	 * 
	 * @see #TYPE_DEFAULT
	 * @see #TYPE_STORAGE
	 * 
	 * @return the type
	 */
	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	/**
	 * The archive creator id
	 * 
	 * @return identifier of the creator
	 */
	public long getCreatorId() {
		return creatorId;
	}

	public void setCreatorId(long creatorId) {
		this.creatorId = creatorId;
	}

	/**
	 * The archive creator name
	 * 
	 * @return name of the creator
	 */
	public String getCreatorName() {
		return creatorName;
	}

	public void setCreatorName(String creatorName) {
		this.creatorName = creatorName;
	}

	/**
	 * The archive closer id
	 * 
	 * @return identifier of the user that closed the archive
	 */
	public Long getCloserId() {
		return closerId;
	}

	public void setCloserId(Long closerId) {
		this.closerId = closerId;
	}

	/**
	 * The archive closer name
	 * 
	 * @return name of the user that closed the archive
	 */
	public String getCloserName() {
		return closerName;
	}

	public void setCloserName(String closerName) {
		this.closerName = closerName;
	}

	/**
	 * The archive's closure date
	 * 
	 * @return the date when the archive has been closed
	 */
	public Date getClosure() {
		return closure;
	}

	public void setClosure(Date closure) {
		this.closure = closure;
	}

	public int getMode() {
		return mode;
	}

	public void setMode(int mode) {
		this.mode = mode;
	}

	public int getImportTemplate() {
		return importTemplate;
	}

	public void setImportTemplate(int importTemplate) {
		this.importTemplate = importTemplate;
	}

	public int getImportCustomId() {
		return importCustomId;
	}

	public void setImportCustomId(int importCustomId) {
		this.importCustomId = importCustomId;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
	}
}