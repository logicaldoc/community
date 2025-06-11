package com.logicaldoc.webservice.model;

import java.util.Date;

import jakarta.xml.bind.annotation.XmlType;

import com.logicaldoc.core.document.DocumentNote;
import com.logicaldoc.webservice.doc.WSDoc;

/**
 * Web Service Note.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.6.3
 */
@XmlType(name = "WSNote")
public class WSNote {

	@WSDoc(description = "unique identifier")
	private long id;

	@WSDoc(description = "the referenced document")
	private long docId;

	@WSDoc(description = "the specific file version")
	private String fileVersion;

	@WSDoc(description = "id of the user that created the note")
	private long userId;

	@WSDoc(description = "name of the user that created the note")
	private String username;

	@WSDoc(description = "name of the document's file")
	private String fileName;
	
	@WSDoc(description = "when the note has been creates")
	private Date date;

	@WSDoc(description = "the text of the note")
	private String message;

	@WSDoc(description = "in which page the note was placed, 0 stays for no page")
	private int page = 0;

	@WSDoc(description = "the opacity")
	private int opacity = 80;

	@WSDoc(description = "the background color")
	private String color = "#FFFF88";

	@WSDoc(description = "left position(0..1)")
	private double left = 0.5;

	@WSDoc(description = "top position(0..1)")
	private double top = 0.5;

	@WSDoc(description = "width(0..1)")
	private double width = 0.15;

	@WSDoc(description = "height(0..1)")
	private double height = 0.10;

	@WSDoc(description = "the opacity of the line")
	private int lineOpacity = 80;

	@WSDoc(description = "the color of the line")
	private String lineColor = "#a1a1a1";

	@WSDoc(description = "rotation(-90, +90)")
	private double rotation = 0.0;
	
	@WSDoc(description = "the width of the line")
	private int lineWidth = 1;

	@WSDoc(description = "shape (square, circle, line, arrow, thickarrow, comment, label)")
	private String shape = "square";
	
	public DocumentNote toDocumentNote() {
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
		note.setShape(getShape());
		note.setLineColor(getLineColor());
		note.setLineOpacity(getLineOpacity());
		note.setLineWidth(getLineWidth());
		note.setRotation(getRotation());
		return note;
	}

	public static WSNote fromDocumentNote(DocumentNote note) {
		WSNote wsNote = new WSNote();
		wsNote.setDate(note.getDate());
		wsNote.setDocId(note.getDocId());
		wsNote.setId(note.getId());
		wsNote.setMessage(note.getMessage());
		wsNote.setUserId(note.getUserId());
		wsNote.setUsername(note.getUsername());
		wsNote.setPage(note.getPage());
		wsNote.setOpacity(note.getOpacity());
		wsNote.setColor(note.getColor());
		wsNote.setTop(note.getTop());
		wsNote.setLeft(note.getLeft());
		wsNote.setWidth(note.getWidth());
		wsNote.setHeight(note.getHeight());
		wsNote.setFileVersion(note.getFileVersion());
		wsNote.setFileName(note.getFileName());
		wsNote.setShape(note.getShape());
		wsNote.setLineColor(note.getLineColor());
		wsNote.setLineOpacity(note.getLineOpacity());
		wsNote.setLineWidth(note.getLineWidth());
		wsNote.setRotation(note.getRotation());
		return wsNote;
	}

	public long getId() {
		return id;
	}

	public void setId(long id) {
		this.id = id;
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

	public String getFileName() {
		return fileName;
	}

	public void setFileName(String fileName) {
		this.fileName = fileName;
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