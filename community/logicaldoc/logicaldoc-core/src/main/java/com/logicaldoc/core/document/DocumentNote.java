package com.logicaldoc.core.document;

import java.util.Date;

import com.logicaldoc.core.PersistentObject;

/**
 * A note over a document
 * 
 * @author Matteo Caruso - LogicalDOC
 * @since 6.2
 */
public class DocumentNote extends PersistentObject implements Cloneable {

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

	@Override
	public Object clone() throws CloneNotSupportedException {
		DocumentNote note = new DocumentNote();
		note.setId(getId());
		note.setDate(getDate());
		note.setDocId(getDocId());
		note.setMessage(getMessage());
		note.setUserId(getUserId());
		note.setUsername(getUsername());
		note.setPage(getPage());
		note.setOpacity(getOpacity());
		note.setColor(getColor());
		note.setTop(getTop());
		note.setLeft(getLeft());
		note.setWidth(getWidth());
		note.setHeight(getHeight());
		note.setFileVersion(getFileVersion());
		note.setFileName(getFileName());
		note.setType(getType());
		note.setRecipient(getRecipient());
		note.setRecipientEmail(getRecipientEmail());
		note.setShape(getShape());
		note.setLineColor(getLineColor());
		note.setLineOpacity(getLineOpacity());
		note.setLineWidth(getLineWidth());
		return note;
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