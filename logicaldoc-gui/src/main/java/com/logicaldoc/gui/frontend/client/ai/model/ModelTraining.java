package com.logicaldoc.gui.frontend.client.ai.model;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.logicaldoc.gui.frontend.client.ai.sampler.SamplerSelector;
import com.smartgwt.client.data.AdvancedCriteria;
import com.smartgwt.client.types.OperatorId;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.SpinnerItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.TextItem;
import com.smartgwt.client.widgets.form.fields.ToggleItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Shows model's training information
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class ModelTraining extends ModelDetailsTab implements ModelObserver {

	private static final String ENABLED = "enabled";

	private DynamicForm form = new DynamicForm();

	private TextAreaItem report;

	private HLayout container = new HLayout();

	private SamplerSelector sampler;

	public ModelTraining(GUIModel model, final ChangedHandler changedHandler) {
		super(model, changedHandler);
		setWidth100();
		setHeight100();

		setMembers(container);

		ModelController.get().addObserver(this);

		refresh();
	}

	private void refresh() {
		if (Boolean.TRUE.equals(container.contains(form)))
			container.removeChild(form);

		ToggleItem enableScheduling = ItemFactory.newToggleItem(ENABLED, "enablescheduling",
				model.getTraining().isEnabled());
		enableScheduling.setWrapTitle(false);
		enableScheduling.addChangedHandler(changedHandler);

		TextItem cron = ItemFactory.newCronExpressionItem("cron", "schedule", model.getTraining().getCron(),
				changedHandler);
		AdvancedCriteria visibleCriteria = new AdvancedCriteria(ENABLED, OperatorId.EQUALS, true);
		cron.setVisibleWhen(visibleCriteria);
		cron.setRequiredWhen(visibleCriteria);

		SpinnerItem epochs = ItemFactory.newSpinnerItem("epochs", model.getTraining().getEpochs());
		epochs.setRequired(true);
		epochs.setStartRow(true);
		epochs.addChangedHandler(changedHandler);

		sampler = new SamplerSelector();
		if (model.getTraining().getSampler() != null)
			sampler.setValue(model.getTraining().getSampler().getId());
		sampler.setRequired(true);
		sampler.addChangedHandler(changedHandler);

		form = new DynamicForm();
		form.setNumCols(1);
		form.setWidth(1);
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setItems(sampler, epochs, enableScheduling, cron);

		container.setWidth100();
		container.setMembersMargin(3);
		container.addMember(form);

		report = ItemFactory.newTextAreaItem("report", model.getTraining().getReport());
		report.setWidth("*");
		onModelChanged(model);

		DynamicForm reportForm = new DynamicForm();
		reportForm.setWidth100();
		reportForm.setNumCols(1);
		reportForm.setTitleOrientation(TitleOrientation.TOP);
		reportForm.setItems(report);
		container.addMember(reportForm);
	}

	boolean validate() {
		if (form.validate()) {
			model.getTraining().setCron(form.getValueAsString("cron"));
			model.getTraining().setEnabled(Boolean.parseBoolean(form.getValueAsString(ENABLED)));
			model.getTraining().setEpochs(Integer.parseInt(form.getValueAsString("epochs")));
			model.getTraining().setSampler(sampler.getSelectedSampler());
		}
		return !form.hasErrors();
	}

	@Override
	public boolean equals(Object other) {
		return super.equals(other);
	}

	@Override
	public int hashCode() {
		return super.hashCode();
	}

	@Override
	public void onModelChanged(GUIModel mdl) {
		if (this.model.getId() == mdl.getId()) {
			report.setValue(mdl.getTraining().getReport());
			report.setTitle(mdl.getTraining().getLastTrained() != null
					? I18N.message("lasttrainedon", I18N.formatDate(mdl.getTraining().getLastTrained()))
					: I18N.message("report"));
		}
	}
}