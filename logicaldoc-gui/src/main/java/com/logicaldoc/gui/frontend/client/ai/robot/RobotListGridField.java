package com.logicaldoc.gui.frontend.client.ai.robot;

import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.grid.ColoredListGridField;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.grid.ListGridRecord;

/**
 * A field to display name and optionally the avatar in cells that contains a
 * robot reference
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class RobotListGridField extends ColoredListGridField {

	boolean displayAvatar = true;

	boolean avoidCaching = false;

	private String avatarFieldName;

	public RobotListGridField(String name) {
		this(name, "avatar");
	}

	public RobotListGridField(String name, String avatarFieldName, String title, boolean displayAvatar) {
		super(name, title != null ? I18N.message(title) : " ");
		this.displayAvatar = displayAvatar;
		this.avatarFieldName = avatarFieldName;
		setCanFilter(true);
		setCanSort(true);
		setCellFormatter(new AvatarCellFormatter(16));
		setCellAlign(Alignment.LEFT);
		setAutoFitWidth(true);
	}

	public RobotListGridField(String name, String avatarFieldName, String title) {
		super(name, title != null ? I18N.message(title) : " ");
		this.avatarFieldName = avatarFieldName;
		setCanFilter(true);
		setCanSort(true);
		setCellFormatter(new AvatarCellFormatter(16));
		setCellAlign(Alignment.LEFT);
		setAutoFitWidth(true);
	}

	public RobotListGridField(String name, String avatarFieldName) {
		this(name, avatarFieldName, null);
	}

	public RobotListGridField() {
		this("name", "avatar", "name");
		setCanFilter(false);
		setCanSort(false);
	}

	public RobotListGridField(boolean avoidCaching) {
		this();
		this.avoidCaching = avoidCaching;
	}

	public boolean isDisplayAvatar() {
		return displayAvatar;
	}

	public void setDisplayAvatar(boolean displayAvatar) {
		this.displayAvatar = displayAvatar;
	}

	public class AvatarCellFormatter extends ColoredCellFormatter {
		int size = Session.get().getConfigAsInt("gui.avatar.size");

		public AvatarCellFormatter() {
			super();
		}

		public AvatarCellFormatter(int size) {
			super();
			this.size = size;
		}

		@Override
		public String format(Object value, ListGridRecord rec, int rowNum, int colNum) {
			if (value == null)
				return "";

			if (displayAvatar) {
				return "<div class='box'><img class='avatarIcon' src='" + rec.getAttributeAsString(avatarFieldName)
						+ "' style='height: " + size + "px; width: " + size + "px' />&nbsp;"
						+ super.format(value, rec, rowNum, colNum) + "</div>";
			} else {
				return super.format(value, rec, rowNum, colNum);
			}
		}
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}
}