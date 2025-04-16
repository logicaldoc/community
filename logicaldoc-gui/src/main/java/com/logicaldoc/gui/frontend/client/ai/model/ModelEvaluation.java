package com.logicaldoc.gui.frontend.client.ai.model;

import com.logicaldoc.gui.common.client.i18n.I18N;
import com.logicaldoc.gui.common.client.util.ItemFactory;
import com.smartgwt.client.types.TitleOrientation;
import com.smartgwt.client.widgets.form.DynamicForm;
import com.smartgwt.client.widgets.form.fields.StaticTextItem;
import com.smartgwt.client.widgets.form.fields.TextAreaItem;
import com.smartgwt.client.widgets.form.fields.events.ChangedHandler;
import com.smartgwt.client.widgets.layout.HLayout;

/**
 * Shows model's evaluation panel.
 * 
 * @author Marco Meschieri - LogicalDOC
 * @since 9.2
 */
public class ModelEvaluation extends ModelDetailsTab implements ModelObserver {

	private DynamicForm form = new DynamicForm();

	private TextAreaItem report;

	private StaticTextItem confusionMatrix;

	private HLayout container = new HLayout();

	public ModelEvaluation(GUIModel model, final ChangedHandler changedHandler) {
		super(model, changedHandler);
		setWidth100();
		setHeight100();

		setMembers(container);

		ModelController.get().addObserver(this);
	}

	@Override
	protected void onDraw() {
		refresh();
	}

	private void refresh() {
		if (Boolean.TRUE.equals(container.contains(form)))
			container.removeChild(form);

		report = ItemFactory.newTextAreaItem("report",
				model.getEvaluation() != null ? model.getEvaluation().getReport() : "");
		report.setWidth("*");

		confusionMatrix = ItemFactory.newStaticTextItem("confusionmatrix",
				model.getEvaluation() != null ? model.getEvaluation().getConfusionMatrix() : "");

		onModelChanged(model);

		form = new DynamicForm();
		form.setNumCols(2);
		form.setWidth100();
		form.setTitleOrientation(TitleOrientation.TOP);
		form.setItems(confusionMatrix, report);

		container.setWidth100();
		container.setMembersMargin(3);
		container.addMember(form);
	}

	boolean validate() {
		return true;
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
		if (this.model.getId() == mdl.getId() && mdl.isNeuralNetwork() && mdl.getEvaluation() != null) {
			report.setValue(mdl.getEvaluation().getReport());
			report.setTitle(mdl.getEvaluation().getLastEvaluated() != null
					? I18N.message("lastevaluatedon", I18N.formatDate(mdl.getEvaluation().getLastEvaluated()))
					: I18N.message("report"));
			confusionMatrix.setValue(mdl.getEvaluation().getConfusionMatrix());
		}
	}
}