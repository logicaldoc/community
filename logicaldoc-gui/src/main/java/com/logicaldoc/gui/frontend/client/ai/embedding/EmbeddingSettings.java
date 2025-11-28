package com.logicaldoc.gui.frontend.client.ai.embedding;

import java.util.ArrayList;
import java.util.Arrays;
import java.util.LinkedHashMap;
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
import com.smartgwt.client.widgets.form.fields.SelectItem;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextItem;

/**
 * This popup window is used to input data and obtain a prediction from the AI
 * model.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class EmbeddingSettings extends Window {

	private static final String BATCH_SETTING = "ai.embedding.batch";

	private static final String SORTING_SETTING = "ai.embedding.sorting";

	private static final String SORTING_CUSTOM_SETTING = "ai.embedding.sorting.custom";

	private static final String THREAD_EMBEDDER_SETTING = "threadpool.Embedder.max";

	private DynamicForm form = new DynamicForm();

	public EmbeddingSettings() {
		setHeaderControls(HeaderControls.HEADER_LABEL, HeaderControls.CLOSE_BUTTON);

		setTitle(I18N.message("settings"));
		setAutoSize(true);
		setCanDragResize(true);
		setIsModal(true);
		setShowModalMask(true);
		centerInPage();

		SettingService.Instance.get().loadSettingsByNames(Arrays.asList(modelsSettingName(), BATCH_SETTING,
				SORTING_SETTING, SORTING_CUSTOM_SETTING, THREAD_EMBEDDER_SETTING), new DefaultAsyncCallback<>() {

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

		SpinnerItem batch = ItemFactory.newSpinnerItem("batch", Integer.parseInt(Util.getValue(BATCH_SETTING, params)));
		batch.setMin(1);
		batch.setStep(1);
		batch.setRequired(true);

		SelectItem sorting = ItemFactory.newSelectItem("sorting");
		LinkedHashMap<String, String> opts = new LinkedHashMap<>();
		opts.put("", I18N.message("none").toLowerCase());
		opts.put("oldestfirst", I18N.message("oldestfirst"));
		opts.put("mostrecentfirst", I18N.message("mostrecentfirst"));
		opts.put("smallestfirst", I18N.message("smallestfirst"));
		opts.put("biggestfirst", I18N.message("biggestfirst"));
		sorting.setValueMap(opts);
		sorting.setValue(Util.getValue(SORTING_SETTING, params));

		String customSortingValue = Util.getValue(SORTING_CUSTOM_SETTING, params);
		sorting.setDisabled(customSortingValue != null && !customSortingValue.isEmpty());
		sorting.setVisible(Session.get().isDefaultTenant());

		TextItem customSorting = ItemFactory.newTextItem("customsorting", customSortingValue);
		customSorting.setWidth(300);
		customSorting.addChangeHandler(changeEvent -> sorting
				.setDisabled(changeEvent.getValue() != null && !changeEvent.getValue().toString().isEmpty()));

		SpinnerItem threads = ItemFactory.newSpinnerItem("threads",
				Integer.parseInt(Util.getValue(THREAD_EMBEDDER_SETTING, params)));
		threads.setMin(1);
		threads.setStep(1);
		threads.setRequired(true);

		form.setNumCols(1);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setFields(batch, sorting, customSorting, threads, save);
		addItem(form);
	}

	private void onSave() {
		if (!form.validate())
			return;
		List<GUIParameter> params = new ArrayList<>();
		params.add(new GUIParameter(BATCH_SETTING, form.getValueAsString("batch")));
		params.add(new GUIParameter(SORTING_SETTING, form.getValueAsString("sorting")));
		params.add(new GUIParameter(SORTING_CUSTOM_SETTING, form.getValueAsString("customsorting")));
		params.add(new GUIParameter(THREAD_EMBEDDER_SETTING, form.getValueAsString("threads")));

		SettingService.Instance.get().saveSettings(params, new EmptyAsyncCallback<>());
		destroy();
	}

	private String modelsSettingName() {
		return Session.get().getTenantName() + ".ai.models";
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