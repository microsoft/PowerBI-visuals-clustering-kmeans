/*
 *  Power BI Visual CLI
 *
 *  Copyright (c) Microsoft Corporation
 *  All rights reserved.
 *  MIT License
 *
 *  Permission is hereby granted, free of charge, to any person obtaining a copy
 *  of this software and associated documentation files (the ""Software""), to deal
 *  in the Software without restriction, including without limitation the rights
 *  to use, copy, modify, merge, publish, distribute, sublicense, and/or sell
 *  copies of the Software, and to permit persons to whom the Software is
 *  furnished to do so, subject to the following conditions:
 *
 *  The above copyright notice and this permission notice shall be included in
 *  all copies or substantial portions of the Software.
 *
 *  THE SOFTWARE IS PROVIDED *AS IS*, WITHOUT WARRANTY OF ANY KIND, EXPRESS OR
 *  IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 *  FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL THE
 *  AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 *  LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
 *  OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER DEALINGS IN
 *  THE SOFTWARE.
 */
module powerbi.extensibility.visual {


    // in order to improve the performance, one can update the <head> only in the initial rendering.
    // set to 'true' if you are using different packages to create the widgets
    const updateHTMLHead: boolean = false;
    const renderVisualUpdateType: number[] = [VisualUpdateType.Resize, VisualUpdateType.ResizeEnd, VisualUpdateType.Resize + VisualUpdateType.ResizeEnd];


    //user edit START: interface objects
    interface VisualSettingsPreprocessingParams {//data preprocessing
       

        scaleData: boolean;
        applyPCA: boolean;
    }

    interface VisualSettingsClusterNumParams {//clustering algo
      
        numOfClusters: string;
        numClustersMethods: string;
    }


    }

    interface VisualSettingsVizParams {//appearance 
        // show: boolean;
        drawEllipse: boolean;
        drawConvexHull: boolean;
        drawCentroid: boolean;
        percentile: number; //TODO: percentage
        weight: number;

    }
    interface VisualSettingsLabelingParams {//points labeling 
        show: boolean;
        //addLabel2points: boolean;
        textSize: number; //TODO: textSize
        //cexLabel2points: number; //TODO: textSize
        percentile: number; //TODO: percentage
        maxLenPointLabel: number;
        percentile1: number; //TODO: percentage

    }
    interface VisualSettingsRepresentativeParams {//representative labeling 
        show: boolean;
        textSize: number;
        maxLenDelegateLabel: number;
    }


    }
    interface VisualSettingsLegendParams {//legend and pallete 
        show: boolean;
        addLegend: boolean;
        palleteType: string;

    }
    interface VisualSettingsAdditionalParams {//additional settings 
        //show: boolean;
        sparsify: boolean;
        minClusters: number;
        maxClusters: number;
        maxIter: number;
        nStart: number;
    }


    export class Visual implements IVisual {
        //    private imageDiv: HTMLDivElement;
        //   private imageElement: HTMLImageElement;
        //HTML
        private rootElement: HTMLElement;
        private headNodes: Node[];
        private bodyNodes: Node[];

         //user edit START: declare private variables
        private settings_prepocessing_params: VisualSettingsPreprocessingParams;
        private settings_clusterNum_params: VisualSettingsClusterNumParams;
        private settings_viz_params: VisualSettingsVizParams;
        private settings_labeling_params: VisualSettingsLabelingParams;
        private settings_representative_params: VisualSettingsRepresentativeParams;
        private settings_legend_params: VisualSettingsLegendParams;
        private settings_additional_params: VisualSettingsAdditionalParams;


        public constructor(options: VisualConstructorOptions) {
            if(options && options.element)
                this.rootElement = options.element;


            this.headNodes = [];
            this.bodyNodes = [];


            //user edit START: default parameters
            this.settings_prepocessing_params = <VisualSettingsPreprocessingParams>{
                // show: false,
                scaleData: false,
                applyPCA: false,
            };

            this.settings_clusterNum_params = <VisualSettingsClusterNumParams>{
                //   show: false,
                numOfClusters: "auto",
                numClustersMethods: "fast"
            };

            this.settings_viz_params = <VisualSettingsVizParams>{
                //show: false,
                drawEllipse: false,
                drawConvexHull: false,
                drawCentroid: false,
                percentile: 40,
                weight: 10
            };

            this.settings_labeling_params = <VisualSettingsLabelingParams>{
                show: true,
                textSize: 8,
                percentile: 80,
                maxLenPointLabel: 5,
                percentile1: 100
            };

            this.settings_representative_params = <VisualSettingsRepresentativeParams>{
                show: false,
                textSize: 8,
                maxLenDelegateLabel: 30
            };

            this.settings_legend_params = <VisualSettingsLegendParams>{
                show: true,
                addLegend: true,
                palleteType: "qPBI"
            };

            this.settings_additional_params = <VisualSettingsAdditionalParams>{
                // show: false,
                sparsify: true,
                minClusters: 2,
                maxClusters: 12,
                maxIter: 10,
                nStart: 5
            };
        }

        public update(options: VisualUpdateOptions) {
            if (!options || !options.type || !options.viewport)
                return;

            let dataViews: DataView[] = options.dataViews;
            if (!dataViews || dataViews.length === 0)
                return;

            let dataView: DataView = dataViews[0];
            if (!dataView || !dataView.metadata)
                return;


            this.updateObjects(dataView.metadata.objects);

            let payloadBase64: string = null;
            if (dataView.scriptResult && dataView.scriptResult.payloadBase64) {
                payloadBase64 = dataView.scriptResult.payloadBase64;
            }

            if (renderVisualUpdateType.indexOf(options.type) === -1) {
                if (payloadBase64) {
                    this.injectCodeFromPayload(payloadBase64);
                }
            }
            
            this.onResizing(options.viewport);
        }

        public onResizing(finalViewport: IViewport): void {
            /* add code to handle resizing of the view port */
        }

      private injectCodeFromPayload(payloadBase64: string): void {
            // Inject HTML from payload, created in R
            // the code is injected to the 'head' and 'body' sections.
            // if the visual was already rendered, the previous DOM elements are cleared

            ResetInjector();

            if (!payloadBase64) 
                return

            // create 'virtual' HTML, so parsing is easier
            let el: HTMLHtmlElement = document.createElement('html');
            try {
                el.innerHTML = window.atob(payloadBase64);
            } catch (err) {
                return;
            }

            // if 'updateHTMLHead == false', then the code updates the header data only on the 1st rendering
            // this option allows loading and parsing of large and recurring scripts only once.
            if (updateHTMLHead || this.headNodes.length === 0) {
                while (this.headNodes.length > 0) {
                    let tempNode: Node = this.headNodes.pop();
                    document.head.removeChild(tempNode);
                }
                let headList: NodeListOf<HTMLHeadElement> = el.getElementsByTagName('head');
                if (headList && headList.length > 0) {
                    let head: HTMLHeadElement = headList[0];
                    this.headNodes = ParseElement(head, document.head);
                }
            }

            // update 'body' nodes, under the rootElement
            while (this.bodyNodes.length > 0) {
                let tempNode: Node = this.bodyNodes.pop();
                this.rootElement.removeChild(tempNode);
            }
            let bodyList: NodeListOf<HTMLBodyElement> = el.getElementsByTagName('body');
            if (bodyList && bodyList.length > 0) {
                let body: HTMLBodyElement = bodyList[0];
                this.bodyNodes = ParseElement(body, this.rootElement);
            }

            RunHTMLWidgetRenderer();
        }


        /**
         * This function gets called by the update function above. You should read the new values of the properties into 
         * your settings object so you can use the new value in the enumerateObjectInstances function below.
         * 
         * Below is a code snippet demonstrating how to expose a single property called "lineColor" from the object called "settings"
         * This object and property should be first defined in the capabilities.json file in the objects section.
         * In this code we get the property value from the objects (and have a default value in case the property is undefined)
         */
        public updateObjects(objects: DataViewObjects) {
           //user edit START: update params
            this.settings_prepocessing_params = <VisualSettingsPreprocessingParams>{
                // show: getValue<boolean>(dataView.metadata.objects, 'settings_prepocessing_params', 'show', false),
                scaleData: getValue<boolean>(objects, 'settings_prepocessing_params', 'scaleData', false),
                applyPCA: getValue<boolean>(objects, 'settings_prepocessing_params', 'applyPCA', false),
            };

            this.settings_clusterNum_params = <VisualSettingsClusterNumParams>{
                //  show: getValue<boolean>(objects, 'settings_clusterNum_params', 'show', false),
                numOfClusters: getValue<string>(objects, 'settings_clusterNum_params', 'numOfClusters', "auto"),
                numClustersMethods: getValue<string>(objects, 'settings_clusterNum_params', 'numClustersMethods', "fast"),
            };
            this.settings_viz_params = <VisualSettingsVizParams>{
                //show: getValue<boolean>(objects, 'settings_viz_params', 'show', false),
                drawEllipse: getValue<boolean>(objects, 'settings_viz_params', 'drawEllipse', false),
                drawConvexHull: getValue<boolean>(objects, 'settings_viz_params', 'drawConvexHull', false),
                drawCentroid: getValue<boolean>(objects, 'settings_viz_params', 'drawCentroid', false),
                percentile: getValue<number>(objects, 'settings_viz_params', 'percentile', 40),
                weight: getValue<number>(objects, 'settings_viz_params', 'weight', 10),
            };

            this.settings_labeling_params = <VisualSettingsLabelingParams>{
                show: getValue<boolean>(objects, 'settings_labeling_params', 'show', true),
                //addLabel2points: getValue<boolean>(objects, 'settings_labeling_params', 'addLabel2points',false),
                textSize: getValue<number>(objects, 'settings_labeling_params', 'textSize', 8),
                percentile: getValue<number>(objects, 'settings_labeling_params', 'percentile', 100),
                maxLenPointLabel: getValue<number>(objects, 'settings_labeling_params', 'maxLenPointLabel', 5),
                percentile1: getValue<number>(objects, 'settings_labeling_params', 'percentile1', 100),
            };
            this.settings_representative_params = <VisualSettingsRepresentativeParams>{
                show: getValue<boolean>(objects, 'settings_representative_params', 'show', false),
                //addLabel2clusterDelegate: getValue<boolean>(objects, 'settings_representative_params', 'addLabel2clusterDelegate', false),  
                textSize: getValue<number>(objects, 'settings_representative_params', 'textSize', 8),
                maxLenDelegateLabel: getValue<number>(objects, 'settings_representative_params', 'maxLenDelegateLabel', 30)

            };
            this.settings_legend_params = <VisualSettingsLegendParams>{
                show: getValue<boolean>(objects, 'settings_legend_params', 'show', true),
                addLegend: getValue<boolean>(objects, 'settings_legend_params', 'addLegend', true),
                palleteType: getValue<string>(objects, 'settings_legend_params', 'palleteType', "qPBI"),
            };

            this.settings_additional_params = <VisualSettingsAdditionalParams>{
                //show: getValue<boolean>(objects, 'settings_additional_params', 'show', false),
                sparsify: getValue<boolean>(objects, 'settings_additional_params', 'sparsify', true),
                minClusters: getValue<number>(objects, 'settings_additional_params', 'minClusters', 2),
                maxClusters: getValue<number>(objects, 'settings_additional_params', 'maxClusters', 12),
                maxIter: getValue<number>(objects, 'settings_additional_params', 'maxIter', 10),
                nStart: getValue<number>(objects, 'settings_additional_params', 'nStart', 5),
            };
            //user edit END: update params
        }
        
        public enumerateObjectInstances(options: EnumerateVisualObjectInstancesOptions): VisualObjectInstanceEnumeration {
            let objectName = options.objectName;
            let objectEnumeration = [];

            switch (objectName) {
                case 'settings_prepocessing_params':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            // show: this.settings_prepocessing_params.show,
                            scaleData: this.settings_prepocessing_params.scaleData,
                            applyPCA: this.settings_prepocessing_params.applyPCA,

                        },
                        selector: null
                    });
                    break;

                case 'settings_clusterNum_params':
                    if (this.settings_clusterNum_params.numOfClusters == "auto") {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                // show: this.settings_clusterNum_params.show,
                                numOfClusters: this.settings_clusterNum_params.numOfClusters,
                                numClustersMethods: ifStringReturnStringClustersMethod(this.settings_clusterNum_params.numClustersMethods, this.settings_clusterNum_params.numOfClusters)
                            },
                            selector: null
                        });
                    }
                    else {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                //  show: this.settings_clusterNum_params.show,
                                numOfClusters: this.settings_clusterNum_params.numOfClusters,
                            },
                            selector: null
                        });
                    }
                    break;



                case 'settings_viz_params':

                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            // show: this.settings_viz_params.show,
                            percentile: this.settings_viz_params.percentile,
                            weight: this.settings_viz_params.weight,
                            drawEllipse: this.settings_viz_params.drawEllipse,
                            drawConvexHull: this.settings_viz_params.drawConvexHull
                            // drawCentroid: this.settings_viz_params.drawCentroid,

                        },
                        selector: null
                    });
                    if (!this.settings_viz_params.drawEllipse) {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                drawCentroid: this.settings_viz_params.drawCentroid
                            },
                            selector: null
                        });

                    }

                    break;
                case 'settings_labeling_params':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings_labeling_params.show,
                            textSize: this.settings_labeling_params.textSize,
                            percentile: this.settings_labeling_params.percentile,
                            maxLenPointLabel: inMinMax(this.settings_labeling_params.maxLenPointLabel, 1, 100),
                            percentile1: inMinMax(this.settings_labeling_params.percentile1, 0, 100)
                        },
                        selector: null
                    });
                    break;
                case 'settings_representative_params':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings_representative_params.show,
                            //addLabel2clusterDelegate: this.settings_representative_params.addLabel2clusterDelegate,
                            textSize: this.settings_representative_params.textSize,
                            maxLenDelegateLabel: inMinMax(this.settings_representative_params.maxLenDelegateLabel, 1, 100),
                        },
                        selector: null
                    });
                    break;
                case 'settings_legend_params':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings_legend_params.show,
                            addLegend: this.settings_legend_params.addLegend,
                            palleteType: this.settings_legend_params.palleteType
                        },
                        selector: null
                    });
                    break;

                case 'settings_additional_params':
                    if (this.settings_clusterNum_params.numOfClusters == "auto") {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                //show: this.settings_additional_params.show,
                              
                                minClusters: inMinMax(this.settings_additional_params.minClusters, 1, 15),
                                maxClusters: inMinMax(this.settings_additional_params.maxClusters, this.settings_additional_params.minClusters, 15),

                                maxIter: inMinMax(this.settings_additional_params.maxIter, 1, 100),
                                nStart: inMinMax(this.settings_additional_params.nStart, 1, 100),
                                 sparsify: this.settings_additional_params.sparsify
                            },
                            selector: null
                        });
                    }
                    else {
                        objectEnumeration.push({
                            objectName: objectName,
                            properties: {
                                // show: this.settings_additional_params.show,
                                maxIter: inMinMax(this.settings_additional_params.maxIter, 1, 100),
                                nStart: inMinMax(this.settings_additional_params.nStart, 1, 100),
                                 sparsify: this.settings_additional_params.sparsify
                            },
                            selector: null
                        });
                    }
                    break;


            };
            //user edit END: populate GUI
            return objectEnumeration;
        }
    }
}