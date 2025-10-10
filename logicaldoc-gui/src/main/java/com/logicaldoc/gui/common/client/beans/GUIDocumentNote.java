package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.Date;
import java.util.List;

/**
 * Representation of a single document handled by the GUI
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.3.4
 */
public class GUIDocumentNote implements Serializable {
	private static final long serialVersionUID = 1L;

	private long id;

	private long docId;

	private String fileVersion;

	private String fileName;

	private long userId;

	private String username;

	private Date date = new Date();

	private String message;

	private int page = 0;

	private int opacity = 70;

	private String color = "#FFCC00";

	private double left = 0.4;

	private double top = 0.35;

	private double width = 0.15;

	private double height = 0.05;

	/**
	 * A type of note, for normal notes in a document it is null
	 */
	private String type;

	/**
	 * A reference to a recipient, it could be a username or the full name of a
	 * person, normally this field is not used
	 */
	private String recipient;

	/**
	 * An email associated to the note, normally this field is not used
	 */
	private String recipientEmail;

	private int lineOpacity = 70;

	private String lineColor = "#a1a1a1";

	private int lineWidth = 1;

	private String shape = "square";

	private double rotation = 0.0;

	private boolean showKnobs = false;

	private boolean movedOrResized = false;

	private List<GUIAccessControlEntry> accessControlList = new ArrayList<>();

	/**
	 * Permissions granted to the current user on this note
	 */
	private GUIAccessControlEntry allowedPermissions = new GUIAccessControlEntry();

	public GUIDocumentNote(long id, GUIDocument doc) {
		super();
		this.id = id;
		this.docId = doc.getId();
		this.fileVersion = doc.getFileVersion();
		this.fileName = doc.getFileName();
	}

	public GUIDocumentNote(long id, String fileVersion) {
		super();
		this.id = id;
		this.fileVersion = fileVersion;
	}

	public GUIDocumentNote(long id) {
		super();
		this.id = id;
	}

	public GUIDocumentNote() {
		super();
	}

	public long getId() {
		return id;
	}

	public long getDocId() {
		return docId;
	}

	public String getFileName() {
		return fileName;
	}

	public long getUserId() {
		return userId;
	}

	public String getUsername() {
		return username;
	}

	public Date getDate() {
		return date;
	}

	public String getMessage() {
		return message;
	}

	public int getPage() {
		return page;
	}

	public void setId(long id) {
		this.id = id;
	}

	public void setDocId(long docId) {
		this.docId = docId;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public void setUserId(long userId) {
		this.userId = userId;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public void setDate(Date date) {
		this.date = date;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public void setPage(int page) {
		this.page = page;
	}

	public int getOpacity() {
		return opacity;
	}

	public String getColor() {
		return color;
	}

	public void setOpacity(int opacity) {
		this.opacity = opacity;
	}

	public void setColor(String color) {
		this.color = color;
	}

	public double getLeft() {
		return left;
	}

	public double getTop() {
		return top;
	}

	public double getWidth() {
		return width;
	}

	public double getHeight() {
		return height;
	}

	public void setLeft(double left) {
		this.left = left;
	}

	public void setTop(double top) {
		this.top = top;
	}

	public void setWidth(double width) {
		this.width = width;
	}

	public void setHeight(double height) {
		this.height = height;
	}

	public String getFileVersion() {
		return fileVersion;
	}

	public void setFileVersion(String fileVersion) {
		this.fileVersion = fileVersion;
	}

	public String getType() {
		return type;
	}

	public void setType(String type) {
		this.type = type;
	}

	public String getRecipient() {
		return recipient;
	}

	public void setRecipient(String recipient) {
		this.recipient = recipient;
	}

	public String getRecipientEmail() {
		return recipientEmail;
	}

	public void setRecipientEmail(String recipientEmail) {
		this.recipientEmail = recipientEmail;
	}

	public int getLineOpacity() {
		return lineOpacity;
	}

	public void setLineOpacity(int lineOpacity) {
		this.lineOpacity = lineOpacity;
	}

	public String getLineColor() {
		return lineColor;
	}

	public void setLineColor(String lineColor) {
		this.lineColor = lineColor;
	}

	public int getLineWidth() {
		return lineWidth;
	}

	public void setLineWidth(int lineWidth) {
		this.lineWidth = lineWidth;
	}

	public String getShape() {
		return shape;
	}

	public void setShape(String shape) {
		this.shape = shape;
	}

	public boolean isSquareShape() {
		return getShape() == null || getShape().isEmpty() || "square".equals(getShape());
	}

	public List<GUIAccessControlEntry> getAccessControlList() {
		return accessControlList;
	}

	public void setAccessControlList(List<GUIAccessControlEntry> accessControlList) {
		this.accessControlList = accessControlList;
	}

	@Override
	public int hashCode() {
		return Long.valueOf(id).hashCode();
	}

	@Override
	public boolean equals(Object obj) {
		if (obj == null)
			return false;

		if (this.getClass() != obj.getClass())
			return false;

		GUIDocumentNote other = (GUIDocumentNote) obj;
		return id == other.id;
	}

	public double getRotation() {
		return rotation;
	}

	public void setRotation(double rotation) {
		this.rotation = rotation;
	}

	public boolean isShowKnobs() {
		return showKnobs;
	}

	public void setShowKnobs(boolean showKnobs) {
		this.showKnobs = showKnobs;
	}

	public boolean isMovedOrResized() {
		return movedOrResized;
	}

	public void setMovedOrResized(boolean movedOrResized) {
		this.movedOrResized = movedOrResized;
	}

	public GUIAccessControlEntry getAllowedPermissions() {
		return allowedPermissions;
	}

	public void setAllowedPermissions(GUIAccessControlEntry allowedPermissions) {
		this.allowedPermissions = allowedPermissions;
	}

	public boolean isRead() {
		return allowedPermissions.isRead();
	}

	public boolean isWrite() {
		return allowedPermissions.isWrite();
	}

	public boolean isDelete() {
		return allowedPermissions.isDelete();
	}

	public boolean isSecurity() {
		return allowedPermissions.isSecurity();
	}
}