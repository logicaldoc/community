package com.logicaldoc.gui.frontend.client.settings;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;

/**
 * This is the form used to show the index check.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 6.0
 */
public class SearchIndexCheckStatus extends Window {

	public SearchIndexCheckStatus(String result) {
		super();
		
		setWidth100();
		setHeight100();

		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);
		setTitle(I18N.message("checkfulltextindex"));
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();
	

		final DynamicForm form = new DynamicForm();
		form.setHeight100();
		form.setWidth100();
		form.setTitleOrientation(TitleOrientation.TOP);

		final TextAreaItem status = new TextAreaItem();
		status.setWidth(700);
		status.setHeight("100%");
		status.setValue(result);
		status.setShowTitle(false);

		form.setFields(status);
		addItem(form);
	}
}