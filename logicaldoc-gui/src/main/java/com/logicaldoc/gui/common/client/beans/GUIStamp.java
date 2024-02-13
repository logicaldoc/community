package com.logicaldoc.gui.common.client.beans;

import java.io.Serializable;
import java.util.ArrayList;
import java.util.List;

/**
 * Implementation of a Stamp
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 7.3
 */
public class GUIStamp extends GUIExtensibleObject implements Serializable {
	private static final long serialVersionUID = 1L;

	private long id = 0L;

	public static final int TYPE_TEXT = 0;

	public static final int TYPE_IMAGE = 1;

	public static final int TYPE_BARCODE = 2;

	public static final int TYPE_HTML = 3;

	public static final int PAGE_OPT_ALL = 0;

	public static final int PAGE_OPT_FIRST = 1;

	public static final int PAGE_OPT_LAST = 2;

	public static final int PAGE_OPT_SEL = 3;

	private int type = TYPE_TEXT;

	private int pageOption = PAGE_OPT_ALL;

	private String pageSelection = "1";

	private int enabled = 1;

	private String name;

	private String description;

	private String text;

	private int opacity = 100;

	private int rotation = 0;

	private int size = 24;

	private String font;

	private String exprX;

	private String exprY;

	private String exprW;

	private String exprH;

	private String barcodeFormat = "CODE_128";

	private int barcodeLabel;

	private String color = "black";

	private int imageWidth = 200;

	private int imageHeight = 150;

	private List<GUIAccessControlEntry> accessControlList = new ArrayList<>();

	private boolean read = true;

	private boolean write = true;

	public GUIStamp() {
		super();
	}

	public GUIStamp(long id) {
		super();
		this.id = id;
	}

	@Override
	public long getId() {
		return id;
	}

	@Override
	public void setId(long id) {
		this.id = id;
	}

	public int getType() {
		return type;
	}

	public void setType(int type) {
		this.type = type;
	}

	public int getEnabled() {
		return enabled;
	}

	public void setEnabled(int enabled) {
		this.enabled = enabled;
	}

	public String getName() {
		return name;
	}

	public void setName(String name) {
		this.name = name;
	}

	public String getDescription() {
		return description;
	}

	public void setDescription(String description) {
		this.description = description;
	}

	public String getText() {
		return text;
	}

	public void setText(String text) {
		this.text = text;
	}

	public int getOpacity() {
		return opacity;
	}

	public void setOpacity(int opacity) {
		this.opacity = opacity;
	}

	public int getRotation() {
		return rotation;
	}

	public void setRotation(int rotation) {
		this.rotation = rotation;
	}

	public String getExprX() {
		return exprX;
	}

	public void setExprX(String exprX) {
		this.exprX = exprX;
	}

	public String getExprY() {
		return exprY;
	}

	public void setExprY(String exprY) {
		this.exprY = exprY;
	}

	public String getColor() {
		return color;
	}

	public void setColor(String color) {
		this.color = color;
	}

	public List<GUIAccessControlEntry> getAccessControlList() {
		return accessControlList;
	}

	public void setAccessControlList(List<GUIAccessControlEntry> accessControlList) {
		this.accessControlList = accessControlList;
	}

	public int getSize() {
		return size;
	}

	public void setSize(int size) {
		this.size = size;
	}

	public double getAspectRatio() {
		if (type == TYPE_TEXT || type == TYPE_HTML)
			return 0;
		else
			return (double) imageWidth / (double) imageHeight;
	}

	public int getPageOption() {
		return pageOption;
	}

	public void setPageOption(int pageOption) {
		this.pageOption = pageOption;
	}

	public String getPageSelection() {
		return pageSelection;
	}

	public void setPageSelection(String pageSelection) {
		this.pageSelection = pageSelection;
	}

	public int getBarcodeLabel() {
		return barcodeLabel;
	}

	public void setBarcodeLabel(int barcodeLabel) {
		this.barcodeLabel = barcodeLabel;
	}

	public String getBarcodeFormat() {
		return barcodeFormat;
	}

	public void setBarcodeFormat(String barcodeFormat) {
		this.barcodeFormat = barcodeFormat;
	}

	public String getExprW() {
		return exprW;
	}

	public String getExprH() {
		return exprH;
	}

	public void setExprW(String exprW) {
		this.exprW = exprW;
	}

	public void setExprH(String exprH) {
		this.exprH = exprH;
	}

	public int getImageWidth() {
		return imageWidth;
	}

	public int getImageHeight() {
		return imageHeight;
	}

	public void setImageWidth(int imageWidth) {
		this.imageWidth = imageWidth;
	}

	public void setImageHeight(int imageHeight) {
		this.imageHeight = imageHeight;
	}

	public String getFont() {
		return font;
	}

	public void setFont(String font) {
		this.font = font;
	}

	public boolean isRead() {
		return read;
	}

	public void setRead(boolean read) {
		this.read = read;
	}

	public boolean isWrite() {
		return write;
	}

	public void setWrite(boolean write) {
		this.write = write;
	}
}