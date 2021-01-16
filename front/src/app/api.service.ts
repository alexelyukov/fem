import { Injectable } from '@angular/core';
import { HttpClient } from '@angular/common/http';

@Injectable()
export class ApiService {
    private url = 'http://localhost:3000';

    constructor(private http: HttpClient) { }

    getThermalFieldRectangleTriangulation(geometry) {
        return this.http.post(`${this.url}/thermal-field-rectangle-triangulation`, {geometry});
    }

    getThermalFieldTubeTriangulation(geometry) {
        return this.http.post(`${this.url}/thermal-field-tube-triangulation`, {geometry});
    }

    getElectrostaticFieldSkinTriangulation(geometry) {
        return this.http.post(`${this.url}/electrostatic-field-skin-triangulation`, {geometry});
    }

    getElectrostaticFieldCordTriangulation(geometry) {
        return this.http.post(`${this.url}/electrostatic-field-cord-triangulation`, {geometry});
    }

    getElectromagneticFieldTransformerTriangulation(geometry) {
        return this.http.post(`${this.url}/electromagnetic-field-transformer-triangulation`, {geometry});
    }

    getElectromagneticFieldMagnetTriangulation(geometry) {
        return this.http.post(`${this.url}/electromagnetic-field-magnet-triangulation`, {geometry});
    }

    getElectrodynamicSystemTriangulation(geometry) {
        return this.http.post(`${this.url}/electrodynamic-system-triangulation`, {geometry});
    }
}