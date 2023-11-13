package com.logicaldoc.gui.common.client.widgets;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.Util;
import com.smartgwt.client.widgets.form.fields.FormItemIcon;

/**
 * An icon to be used to copy into the clipboard the value of a form item
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 8.9
 */
public class CopyTextFormItemIcon extends FormItemIcon {
	/**
	 * The constructor
	 * 
	 * @param text the text to copy into the clipboard
	 */
	public CopyTextFormItemIcon(String text) {
		setPrompt(I18N.message("copytext"));
		setSrc("[SKIN]/page_white_paste.png");
		setWidth(16);
		setHeight(16);
		addFormItemClickHandler(event -> Util.copyText(text));
	}
}
