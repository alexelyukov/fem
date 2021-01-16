import { Component, ViewChild } from '@angular/core';
import { ApiService } from '../api.service';
import { Geometry, Triangulation, Voronoi } from '../utils';
import { ElectromagneticFieldTransformerPixiComponent } from './electromagnetic-field-transformer-pixi.component';

@Component({
  selector: 'electromagnetic-field-transformer',
  templateUrl: './electromagnetic-field-transformer.component.html',
  styleUrls: ['./electromagnetic-field-transformer.component.sass']
})
export class ElectromagneticFieldTransformerComponent {
  public triangulation: Triangulation = null;
  public voronoi: Voronoi = null

  public isTriangulationWaiting = false;
  public isSolveWaiting = false;
  public isActiveTriangulationBtn = true;
  public isActiveSolveBtn = false;
  public isActiveClearBtn = false;

  public showTriangulation = true;
  public showVoronoi = false;

  private geometry: Geometry = null;

  @ViewChild(ElectromagneticFieldTransformerPixiComponent, {static: false})
  private canvas: ElectromagneticFieldTransformerPixiComponent;

  constructor(private apiService: ApiService) { }

  getTriangulation() {
    this.isTriangulationWaiting = true;
    this.apiService.getElectromagneticFieldTransformerTriangulation(this.geometry)
      .subscribe(
        (data: string) => {
          const parsedData: {triangulation: Triangulation, voronoi: Voronoi} = JSON.parse(data);

          this.triangulation = parsedData.triangulation;
          this.voronoi = parsedData.voronoi;
          this.isActiveSolveBtn = true;
          this.isActiveClearBtn = true;
          this.isActiveTriangulationBtn = false;

          this.canvas.drawTriangulation(this.triangulation);

          console.log(this.triangulation, this.voronoi);

          this.isTriangulationWaiting = false;
        },
        (error: Error) => {
          console.log(error);
          this.isTriangulationWaiting = false;
        }
      );
  }

  onNotify(geometry: Geometry) {
    this.geometry = geometry;
  }

  solve() {
    console.log("Запрос на решение");
  }

  clear() {
    this.triangulation = null;
    this.voronoi = null;

    this.canvas.clearAll();

    this.isActiveSolveBtn = false;
    this.isActiveClearBtn = false;
    this.isActiveTriangulationBtn = true;

    this.showTriangulation = true;
    this.showVoronoi = false;
  }

  change() {
    this.canvas.clearAll();

    if (this.showTriangulation) {
      this.canvas.drawTriangulation(this.triangulation);
    }

    if (this.showVoronoi) {
      this.canvas.drawVoronoi(this.voronoi);
    }
  }
}