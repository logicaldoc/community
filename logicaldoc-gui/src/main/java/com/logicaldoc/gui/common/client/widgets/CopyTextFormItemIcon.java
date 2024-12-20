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
	 * The constructor.
	 * 
	 * @param text the text to copy into the clipboard
	 * @param tooltip the tooltip to display
	 */
	public CopyTextFormItemIcon(String text, String tooltip) {
		setName("copy");
		setPrompt(I18N.message(tooltip));
		setSrc("[SKIN]/paste.svg");
		setWidth(16);
		setHeight(16);
		addFormItemClickHandler(event -> {
			if (text != null)
				Util.copyText(text);
			else
				Util.copyText(event.getItem().getValue().toString());
		});
	}

	/**
	 * The constructor.
	 * 
	 * @param text the text to copy into the clipboard
	 */
	public CopyTextFormItemIcon(String text) {
		this(text, "copytext");
	}

	/**
	 * The constructor, the current item's text will be copied into the
	 * clipboard.
	 */
	public CopyTextFormItemIcon() {
		this(null);
	}
}
