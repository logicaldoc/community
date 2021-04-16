package com.logicaldoc.gui.common.client.util;

import com.logicaldoc.gui.common.client.Constants;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.Alignment;
import com.smartgwt.client.widgets.menu.Menu;
import com.smartgwt.client.widgets.toolbar.ToolStripButton;
import com.smartgwt.client.widgets.toolbar.ToolStripMenuButton;

/**
 * Factory of objects that make use of font awesome
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.0
 */
public class AwesomeFactory {
	/**
	 * Creates a ToolStripButton using font-awesome icon
	 * 
	 * @param icon the icon file name
	 * @param toolTip the message to display when the user moves the cursor over
	 *        the button
	 * 
	 * @return the new button
	 */
	public static ToolStripButton newToolStripButton(String icon, String toolTip) {
		return newToolStripButton(icon, toolTip, null);
	}

	/**
	 * Creates a ToolStripButton using font-awesome icon
	 * 
	 * @param icon the icon file name
	 * @param toolTip the message to display when the user moves the cursor over
	 *        the button
	 * @param text title of the button
	 * 
	 * @return the new button
	 */
	public static ToolStripButton newToolStripButton(String icon, String toolTip, String text) {
		ToolStripButton button = new ToolStripButton();
		button.setTooltip(I18N.message(toolTip));
		button.setTitle(getIconHtml(icon, text));
		button.setAutoFit(true);
		return button;
	}

	/**
	 * Creates a ToolStripButton using font-awesome icon
	 * 
	 * @param icon the icon file name
	 * @param rotation rotation specification for the icon
	 * @param title title of the button
	 * @param toolTip the message to display when the user moves the cursor over
	 *        the button
	 * @param menu the menu to display
	 * 
	 * @return the new button
	 */
	public static ToolStripMenuButton newToolStripToolStripMenuButton(String icon, String rotation, String title,
			String toolTip, Menu menu) {
		ToolStripMenuButton menuButton = new ToolStripMenuButton(AwesomeFactory.getIconHtml(icon), menu);
		menuButton.setTooltip(I18N.message(toolTip));
		menuButton.setTitle(getIconHtml(icon, rotation, title));
		menuButton.setAutoFit(true);
		return menuButton;
	}

	/**
	 * Creates a ToolStripButton using font-awesome icon useful for small icons
	 * (16x16)
	 * 
	 * @param icon the icon file name
	 * @param toolTip the message to display when the user moves the cursor over
	 *        the button
	 * 
	 * @return the new button
	 */
	public static ToolStripButton newIconButton(String icon, String toolTip) {
		return newIconButton(icon, toolTip, null);
	}

	/**
	 * Creates a ToolStripButton using font-awesome icon useful for small icons
	 * (16x16)
	 * 
	 * @param icon the icon file name
	 * @param toolTip the message to display when the user moves the cursor over
	 *        the button
	 * @param text title of the button
	 * 
	 * @return the new button
	 */
	public static ToolStripButton newIconButton(String icon, String toolTip, String text) {
		ToolStripButton button = AwesomeFactory.newToolStripButton(icon, toolTip, text);
		button.setShowDown(false);
		button.setShowRollOver(false);
		button.setLayoutAlign(Alignment.CENTER);
		button.setHeight(16);
		button.setWidth(16);
		button.setMargin(0);
		return button;
	}

	public static ToolStripButton newLockedButton(Integer status, String user) {
		ToolStripButton button = AwesomeFactory.newIconButton("edit", "");
		button.setTitle(DocUtil.getLockedIcon(status));

		String prompt = "";
		if (status == Constants.DOC_CHECKED_OUT || status == Constants.DOC_LOCKED)
			prompt = I18N.message("lockedby") + " " + user;

		button.setPrompt(prompt);

		return button;
	}

	public static ToolStripButton newIndexedButton(Integer indexed) {
		ToolStripButton button = AwesomeFactory.newIconButton("database", "indexed");
		button.setTitle(DocUtil.getIndexedIcon(indexed));
		return button;
	}

	public static String getCssClassPrefix() {
		return "fa" + (Util.isCommunity() ? "s" : "l");
	}

	public static String getIconHtml(String icon) {
		return getIconHtml(icon, null);
	}

	public static String getSpinnerIconHtml(String icon, String text) {
		if (text == null || text.isEmpty())
			return "<i class='" + getCssClassPrefix() + " fa-" + icon + " fa-lg fa-spinner' aria-hidden='true'></i>";
		else
			return "<div><i class='" + getCssClassPrefix() + " fa-" + icon
					+ " fa-lg fa-fw fa-spinner' aria-hidden='true'></i>&nbsp;" + I18N.message(text) + "</div>";
	}

	public static String getIconHtml(String icon, String text) {
		if (text == null || text.isEmpty())
			return "<i class='" + getCssClassPrefix() + " fa-" + icon + " fa-lg' aria-hidden='true'></i>";
		else
			return "<div><i class='" + getCssClassPrefix() + " fa-" + icon + " fa-lg fa-fw' aria-hidden='true'></i> "
					+ I18N.message(text) + "</div>";
	}

	public static String getIconHtml(String icon, String rotation, String text) {
		if (text == null || text.isEmpty())
			return "<i class='" + getCssClassPrefix() + " fa-" + icon + (rotation != null ? " " + rotation : "")
					+ " fa-lg' aria-hidden='true'></i>";
		else
			return "<div><i class='" + getCssClassPrefix() + " fa-" + icon + (rotation != null ? " " + rotation : "")
					+ " fa-lg fa-fw' aria-hidden='true'></i> " + I18N.message(text) + "</div>";
	}
}