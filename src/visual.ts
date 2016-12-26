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
    
    interface VisualSettingsPreprocessingParams {//data preprocessing
        show: boolean;
        scaleData: boolean;      
        applyPCA: boolean;            
    }

     interface VisualSettingsClusterNumParams {//clustering algo
        show: boolean;
        numOfClusters: string;
        numClustersMethods: string;

     }

     interface VisualSettingsVizParams {//appearance 
        show: boolean;
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
        textSize: number; //TODO: textSize
        maxLenDelegateLabel: number;

     }
     interface VisualSettingsLegendParams {//legend and pallete 
        show: boolean;
        addLegend: boolean;
        palleteType: string; //TODO

     }
     interface VisualSettingsAdditionalParams {//additional settings 
        show: boolean;
         showWarnings: boolean;
         minClusters: number;
         maxClusters: number;
         maxIter: number;
         nStart: number;
     }

    

    export class Visual implements IVisual {
        private imageDiv: HTMLDivElement;
        private imageElement: HTMLImageElement;

        private settings_prepocessing_params: VisualSettingsPreprocessingParams;
        private settings_clusterNum_params: VisualSettingsClusterNumParams;
        private settings_viz_params: VisualSettingsVizParams;
        private settings_labeling_params: VisualSettingsLabelingParams;
        private settings_representative_params: VisualSettingsRepresentativeParams;
        private settings_legend_params: VisualSettingsLegendParams;
        private settings_additional_params: VisualSettingsAdditionalParams;
      

        public constructor(options: VisualConstructorOptions) {
            this.imageDiv = document.createElement('div');
            this.imageDiv.className = 'rcv_autoScaleImageContainer';
            options.element.appendChild(this.imageDiv);
            
            this.imageElement = document.createElement('img');
            this.imageElement.className = 'rcv_autoScaleImage';

            this.imageDiv.appendChild(this.imageElement);

             this.settings_prepocessing_params = <VisualSettingsPreprocessingParams>{
                show: false,
                 scaleData: false,
                 applyPCA: false,
            };
            this.settings_clusterNum_params = <VisualSettingsClusterNumParams>{
                show: false,
                numOfClusters: "auto",
                numClustersMethods: "fast",
            };
            this.settings_viz_params = <VisualSettingsVizParams>{
                show: false,
                drawEllipse: false,
                drawConvexHull: false,
                drawCentroid: false,
                percentile: 40,
                weight: 10,
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
                palleteType: "rainbow"
                
            };
            this.settings_additional_params = <VisualSettingsAdditionalParams>{
                show: false,
                showWarnings: true,
                minClusters: 2,
                maxClusters: 12,
                maxIter: 10,
                nStart: 5,
            };
            
        }

        public update(options: VisualUpdateOptions) {
            let dataViews: DataView[] = options.dataViews;
            if (!dataViews || dataViews.length === 0)
                return;

            let dataView: DataView = dataViews[0];
            if (!dataView || !dataView.metadata)
                return;

            this.settings_prepocessing_params = <VisualSettingsPreprocessingParams> {
                show: getValue<boolean>(dataView.metadata.objects, 'settings_prepocessing_params', 'show', false),
                scaleData: getValue<boolean>(dataView.metadata.objects, 'settings_prepocessing_params', 'scaleData', false),              
                applyPCA: getValue<boolean>(dataView.metadata.objects, 'settings_prepocessing_params', 'applyPCA',false),
            };

            this.settings_clusterNum_params= <VisualSettingsClusterNumParams> {
                show: getValue<boolean>(dataView.metadata.objects, 'settings_clusterNum_params', 'show', false),
                numOfClusters: getValue<string>(dataView.metadata.objects, 'settings_clusterNum_params', 'numOfClusters',"auto"),
                numClustersMethods: getValue<string>(dataView.metadata.objects, 'settings_clusterNum_params', 'numClustersMethods',"fast"),
            };
            this.settings_viz_params = <VisualSettingsVizParams> {
                show: getValue<boolean>(dataView.metadata.objects, 'settings_viz_params', 'show', false),
                drawEllipse: getValue<boolean>(dataView.metadata.objects, 'settings_viz_params', 'drawEllipse', false),
                drawConvexHull: getValue<boolean>(dataView.metadata.objects, 'settings_viz_params', 'drawConvexHull', false),
                drawCentroid: getValue<boolean>(dataView.metadata.objects, 'settings_viz_params', 'drawCentroid', false),
                percentile: getValue<number>(dataView.metadata.objects, 'settings_viz_params', 'percentile',40),
                weight: getValue<number>(dataView.metadata.objects, 'settings_viz_params', 'weight',10),
            };

            this.settings_labeling_params = <VisualSettingsLabelingParams> {
                show: getValue<boolean>(dataView.metadata.objects, 'settings_labeling_params', 'show', true),
               //addLabel2points: getValue<boolean>(dataView.metadata.objects, 'settings_labeling_params', 'addLabel2points',false),
                textSize: getValue<number>(dataView.metadata.objects, 'settings_labeling_params', 'textSize',8),
                percentile: getValue<number>(dataView.metadata.objects, 'settings_labeling_params', 'percentile',100),
                maxLenPointLabel: getValue<number>(dataView.metadata.objects, 'settings_labeling_params', 'maxLenPointLabel',5),
                percentile1: getValue<number>(dataView.metadata.objects, 'settings_labeling_params', 'percentile1',100),
            };
            this.settings_representative_params = <VisualSettingsRepresentativeParams> {
                show: getValue<boolean>(dataView.metadata.objects, 'settings_representative_params', 'show', false),
                //addLabel2clusterDelegate: getValue<boolean>(dataView.metadata.objects, 'settings_representative_params', 'addLabel2clusterDelegate', false),  
                textSize: getValue<number>(dataView.metadata.objects, 'settings_representative_params', 'textSize', 8),
                maxLenDelegateLabel: getValue<number>(dataView.metadata.objects, 'settings_representative_params', 'maxLenDelegateLabel', 30)

            };
            this.settings_legend_params = <VisualSettingsLegendParams> {
                show: getValue<boolean>(dataView.metadata.objects, 'settings_legend_params', 'show', true),
                addLegend: getValue<boolean>(dataView.metadata.objects, 'settings_legend_params', 'addLegend', true),
                palleteType: getValue<string>(dataView.metadata.objects, 'settings_legend_params', 'palleteType', "rainbow"),
            };

            this.settings_additional_params = <VisualSettingsAdditionalParams> {
                show: getValue<boolean>(dataView.metadata.objects, 'settings_additional_params', 'show', false),
                showWarnings: getValue<boolean>(dataView.metadata.objects, 'settings_additional_params', 'showWarnings', true),
                minClusters: getValue<number>(dataView.metadata.objects, 'settings_additional_params', 'minClusters', 2),
                maxClusters: getValue<number>(dataView.metadata.objects, 'settings_additional_params', 'maxClusters', 12),
                maxIter: getValue<number>(dataView.metadata.objects, 'settings_additional_params', 'maxIter', 10),
                nStart: getValue<number>(dataView.metadata.objects, 'settings_additional_params', 'nStart', 5),
            };
      
           

            let imageUrl: string = null;
            if (dataView.scriptResult && dataView.scriptResult.payloadBase64) {
                imageUrl = "data:image/png;base64," + dataView.scriptResult.payloadBase64;
            }

            if (imageUrl) {
                this.imageElement.src = imageUrl;
            } else {
                this.imageElement.src = null;
            }

            this.onResizing(options.viewport);
        }

        public onResizing(finalViewport: IViewport): void {
            this.imageDiv.style.height = finalViewport.height + 'px';
            this.imageDiv.style.width = finalViewport.width + 'px';
        }

        public enumerateObjectInstances(options: EnumerateVisualObjectInstancesOptions): VisualObjectInstanceEnumeration {
            let objectName = options.objectName;
            let objectEnumeration = [];

            switch(objectName) {
                case 'settings_prepocessing_params':
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings_prepocessing_params.show,
                            scaleData: this.settings_prepocessing_params.scaleData,
                            applyPCA: this.settings_prepocessing_params.applyPCA,
                            
                         },
                        selector: null
                    });
                    break;
                    
                    case 'settings_clusterNum_params':
                    if(this.settings_clusterNum_params.numOfClusters=="auto")
                    {
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings_clusterNum_params.show,
                            numOfClusters: this.settings_clusterNum_params.numOfClusters,
                            numClustersMethods:ifStringReturnStringClustersMethod(this.settings_clusterNum_params.numClustersMethods, this.settings_clusterNum_params.numOfClusters)
                         },
                        selector: null
                    });
                    }
                    else
                    {
                       objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings_clusterNum_params.show,
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
                            show: this.settings_viz_params.show,
                            drawEllipse: this.settings_viz_params.drawEllipse,
                            drawConvexHull: this.settings_viz_params.drawConvexHull,
                            drawCentroid: this.settings_viz_params.drawCentroid,
                            percentile: this.settings_viz_params.percentile,
                            weight: this.settings_viz_params.weight
                         },
                        selector: null
                    });
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
                     if(this.settings_clusterNum_params.numOfClusters=="auto")
                    {
                    objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings_additional_params.show,
                            showWarnings: this.settings_additional_params.showWarnings,
                          
                            minClusters: inMinMax(this.settings_additional_params.minClusters, 1, 15),
                            maxClusters: inMinMax(this.settings_additional_params.maxClusters, this.settings_additional_params.minClusters, 15),
                          
                            maxIter: inMinMax(this.settings_additional_params.maxIter, 1, 100),
                             nStart: inMinMax(this.settings_additional_params.nStart, 1, 100),
                         },
                        selector: null
                    });
                    }
                    else
                    {
                      objectEnumeration.push({
                        objectName: objectName,
                        properties: {
                            show: this.settings_additional_params.show,
                            showWarnings: this.settings_additional_params.showWarnings,   
                            maxIter: inMinMax(this.settings_additional_params.maxIter, 1, 100),
                             nStart: inMinMax(this.settings_additional_params.nStart, 1, 100),
                         },
                        selector: null
                    });  
                    }
                    break;
                    
            };

            return objectEnumeration;
        }
    }
}