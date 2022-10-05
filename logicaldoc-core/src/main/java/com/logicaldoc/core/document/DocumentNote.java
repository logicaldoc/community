package com.logicaldoc.core.document;

import java.util.Date;

import com.logicaldoc.core.PersistentObject;

/**
 * A note over a document
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
public class DocumentNote extends PersistentObject {

	private static final long serialVersionUID = 1L;

	public static final String SHAPE_SQUARE = "square";

	public static final String SHAPE_CIRCLE = "circle";

	public static final String SHAPE_LINE = "line";

	public static final String SHAPE_ARROW = "arrow";

	public static final String SHAPE_THICKARROW = "thickarrow";

	public static final String SHAPE_COMMENT = "comment";

	public static final String SHAPE_LABEL = "label";

	private long docId;

	private String fileVersion;

	private String fileName;

	/**
	 * Unique identifier of the user that created this note
	 */
	private long userId;

	/**
	 * Unique username of the user that created this note
	 */
	private String username;

	private Date date = new Date();

	private String message;

	private int page = 0;

	private int opacity = 80;

	private String color = "#FFCC00";

	private double left = 0.5;

	private double top = 0.5;

	private double width = 0.15;

	private double height = 0.10;

	private int lineOpacity = 80;

	private String lineColor = "#a1a1a1";

	private int lineWidth = 1;

	private String shape = SHAPE_SQUARE;

	private double rotation = 0.0;

	/**
	 * A type of note, for normal notes it is null
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

	public DocumentNote() {
	}

	public DocumentNote(DocumentNote source) {
		this.docId = source.docId;
		this.fileVersion = source.fileVersion;
		this.fileName = source.fileName;
		this.userId = source.userId;
		this.username = source.username;
		this.date = source.date;
		this.message = source.message;
		this.page = source.page;
		this.opacity = source.opacity;
		this.color = source.color;
		this.left = source.left;
		this.top = source.top;
		this.width = source.width;
		this.height = source.height;
		this.lineOpacity = source.lineOpacity;
		this.lineColor = source.lineColor;
		this.lineWidth = source.lineWidth;
		this.shape = source.shape;
		this.rotation = source.rotation;
		this.type = source.type;
		this.recipient = source.recipient;
		this.recipientEmail = source.recipientEmail;

		setFileVersion(source.getFileVersion());
	}

	public long getDocId() {
		return docId;
	}

	public void setDocId(long docId) {
		this.docId = docId;
	}

	public long getUserId() {
		return userId;
	}

	public void setUserId(long userId) {
		this.userId = userId;
	}

	public String getUsername() {
		return username;
	}

	public void setUsername(String username) {
		this.username = username;
	}

	public Date getDate() {
		return date;
	}

	public void setDate(Date date) {
		this.date = date;
	}

	public String getMessage() {
		return message;
	}

	public void setMessage(String message) {
		this.message = message;
	}

	public int getPage() {
		return page;
	}

	public void setPage(int page) {
		this.page = page;
	}

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
	}

	public int getOpacity() {
		return opacity;
	}

	public String getColor() {
		return color;
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

	public void setOpacity(int opacity) {
		this.opacity = opacity;
	}

	public void setColor(String color) {
		this.color = color;
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

	public double getRotation() {
		return rotation;
	}

	public void setRotation(double rotation) {
		this.rotation = rotation;
	}
}