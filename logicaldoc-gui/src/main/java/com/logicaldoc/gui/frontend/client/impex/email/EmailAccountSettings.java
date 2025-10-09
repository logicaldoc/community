package com.logicaldoc.gui.frontend.client.impex.email;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.List;

import com.logicaldoc.gui.common.client.DefaultAsyncCallback;
import com.logicaldoc.gui.common.client.EmptyAsyncCallback;
import com.logicaldoc.gui.common.client.Session;
import com.logicaldoc.gui.common.client.beans.GUIParameter;
import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.common.client.util.Util;
import com.logicaldoc.gui.frontend.client.services.SettingService;
import com.smartgwt.client.types.HeaderControls;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.Window;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.ButtonItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;

/**
 * This popup window is used to edit general settings related to Email Accounts
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class EmailAccountSettings extends Window {

	private static final String THREADS_SETTING = "threadpool.EmailCrawler.max";

	private DynamicForm form = new DynamicForm();

	public EmailAccountSettings() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		setTitle(I18N.message("settings"));
		setAutoSize(true);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		SettingService.Instance.get().loadSettingsByNames(Arrays.asList(THREADS_SETTING), new DefaultAsyncCallback<>() {

			@Override
			public void handleSuccess(List<GUIParameter> params) {
				init(params);
			}
		});
	}

	private void init(List<GUIParameter> params) {
		ButtonItem save = new ButtonItem();
		save.setTitle(I18N.message("save"));
		save.setAutoFit(true);
		save.setStartRow(true);
		save.addClickHandler(event -> onSave());

		SpinnerItem threads = ItemFactory.newSpinnerItem("threads",
				Integer.parseInt(Util.getValue(THREADS_SETTING, params)));
		threads.setRequired(true);
		threads.setMin(1);
		threads.setStep(1);
		threads.setVisible(Session.get().isDefaultTenant());

		form.setNumCols(1);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setFields(threads, save);

		addItem(form);
	}

	private void onSave() {
		if (!form.validate())
			return;

		List<GUIParameter> params = new ArrayList<>();
		params.add(new GUIParameter(THREADS_SETTING, form.getValueAsString("threads")));
		SettingService.Instance.get().saveSettings(params, new EmptyAsyncCallback<>());

		destroy();
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