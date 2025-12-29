//! Fuzzy Logic Integration Module
//!
//! This module provides fuzzy logic capabilities for N3 reasoning:
//! - Fuzzy membership functions
//! - Fuzzy sets and operations
//! - Fuzzy rules and inference
//! - Defuzzification methods
//! - Linguistic variables

use crate::term::{Term, Triple};
use std::collections::HashMap;

/// A fuzzy truth value in [0, 1]
#[derive(Debug, Clone, Copy, PartialEq)]
pub struct FuzzyValue(f64);

impl FuzzyValue {
    pub fn new(value: f64) -> Self {
        Self(value.clamp(0.0, 1.0))
    }

    pub fn value(&self) -> f64 {
        self.0
    }

    /// Fuzzy NOT (complement)
    pub fn not(&self) -> Self {
        Self::new(1.0 - self.0)
    }

    /// Fuzzy AND (t-norm) - minimum
    pub fn and(&self, other: &Self) -> Self {
        Self::new(self.0.min(other.0))
    }

    /// Fuzzy OR (t-conorm) - maximum
    pub fn or(&self, other: &Self) -> Self {
        Self::new(self.0.max(other.0))
    }

    /// Algebraic product t-norm
    pub fn product(&self, other: &Self) -> Self {
        Self::new(self.0 * other.0)
    }

    /// Bounded sum t-conorm
    pub fn bounded_sum(&self, other: &Self) -> Self {
        Self::new((self.0 + other.0).min(1.0))
    }

    /// Łukasiewicz t-norm
    pub fn lukasiewicz_and(&self, other: &Self) -> Self {
        Self::new((self.0 + other.0 - 1.0).max(0.0))
    }

    /// Łukasiewicz t-conorm
    pub fn lukasiewicz_or(&self, other: &Self) -> Self {
        Self::new((self.0 + other.0).min(1.0))
    }

    /// Fuzzy implication (Mamdani: min)
    pub fn implies_mamdani(&self, other: &Self) -> Self {
        self.and(other)
    }

    /// Fuzzy implication (Larsen: product)
    pub fn implies_larsen(&self, other: &Self) -> Self {
        self.product(other)
    }

    /// Check if this is "true enough" given a threshold
    pub fn is_true(&self, threshold: f64) -> bool {
        self.0 >= threshold
    }
}

impl Default for FuzzyValue {
    fn default() -> Self {
        Self(0.0)
    }
}

impl From<f64> for FuzzyValue {
    fn from(v: f64) -> Self {
        Self::new(v)
    }
}

impl From<bool> for FuzzyValue {
    fn from(b: bool) -> Self {
        Self::new(if b { 1.0 } else { 0.0 })
    }
}

/// Membership function types
#[derive(Debug, Clone)]
pub enum MembershipFunction {
    /// Triangular: (left, center, right)
    Triangular(f64, f64, f64),
    /// Trapezoidal: (left, left_top, right_top, right)
    Trapezoidal(f64, f64, f64, f64),
    /// Gaussian: (center, sigma)
    Gaussian(f64, f64),
    /// Sigmoid: (center, slope)
    Sigmoid(f64, f64),
    /// S-shaped: (left, right)
    SCurve(f64, f64),
    /// Z-shaped: (left, right)
    ZCurve(f64, f64),
    /// Bell-shaped: (center, width, slope)
    Bell(f64, f64, f64),
    /// Singleton at a specific value
    Singleton(f64),
    /// Custom function via lookup table
    Custom(Vec<(f64, f64)>),
}

impl MembershipFunction {
    /// Evaluate membership for a crisp value
    pub fn evaluate(&self, x: f64) -> FuzzyValue {
        let result = match self {
            MembershipFunction::Triangular(a, b, c) => {
                if x <= *a || x >= *c {
                    0.0
                } else if x <= *b {
                    (x - a) / (b - a)
                } else {
                    (c - x) / (c - b)
                }
            }
            MembershipFunction::Trapezoidal(a, b, c, d) => {
                if x <= *a || x >= *d {
                    0.0
                } else if x >= *b && x <= *c {
                    1.0
                } else if x < *b {
                    (x - a) / (b - a)
                } else {
                    (d - x) / (d - c)
                }
            }
            MembershipFunction::Gaussian(c, sigma) => {
                let exp = -((x - c).powi(2)) / (2.0 * sigma.powi(2));
                exp.exp()
            }
            MembershipFunction::Sigmoid(c, a) => 1.0 / (1.0 + (-a * (x - c)).exp()),
            MembershipFunction::SCurve(a, b) => {
                if x <= *a {
                    0.0
                } else if x >= *b {
                    1.0
                } else {
                    let mid = (a + b) / 2.0;
                    if x <= mid {
                        2.0 * ((x - a) / (b - a)).powi(2)
                    } else {
                        1.0 - 2.0 * ((x - b) / (b - a)).powi(2)
                    }
                }
            }
            MembershipFunction::ZCurve(a, b) => {
                if x <= *a {
                    1.0
                } else if x >= *b {
                    0.0
                } else {
                    let mid = (a + b) / 2.0;
                    if x <= mid {
                        1.0 - 2.0 * ((x - a) / (b - a)).powi(2)
                    } else {
                        2.0 * ((x - b) / (b - a)).powi(2)
                    }
                }
            }
            MembershipFunction::Bell(c, a, b) => {
                1.0 / (1.0 + ((x - c) / a).abs().powf(2.0 * b))
            }
            MembershipFunction::Singleton(v) => {
                if (x - v).abs() < f64::EPSILON {
                    1.0
                } else {
                    0.0
                }
            }
            MembershipFunction::Custom(points) => {
                if points.is_empty() {
                    0.0
                } else {
                    // Linear interpolation
                    let mut result = None;
                    for i in 0..points.len() - 1 {
                        if x >= points[i].0 && x <= points[i + 1].0 {
                            let t = (x - points[i].0) / (points[i + 1].0 - points[i].0);
                            result = Some(points[i].1 + t * (points[i + 1].1 - points[i].1));
                            break;
                        }
                    }
                    result.unwrap_or_else(|| {
                        if x < points[0].0 {
                            points[0].1
                        } else {
                            points.last().unwrap().1
                        }
                    })
                }
            }
        };

        FuzzyValue::new(result)
    }

    /// Get the core (where membership = 1)
    pub fn core(&self) -> Option<(f64, f64)> {
        match self {
            MembershipFunction::Triangular(_, b, _) => Some((*b, *b)),
            MembershipFunction::Trapezoidal(_, b, c, _) => Some((*b, *c)),
            MembershipFunction::Gaussian(c, _) => Some((*c, *c)),
            MembershipFunction::Singleton(v) => Some((*v, *v)),
            _ => None,
        }
    }

    /// Get the support (where membership > 0)
    pub fn support(&self) -> Option<(f64, f64)> {
        match self {
            MembershipFunction::Triangular(a, _, c) => Some((*a, *c)),
            MembershipFunction::Trapezoidal(a, _, _, d) => Some((*a, *d)),
            MembershipFunction::Singleton(v) => Some((*v, *v)),
            _ => None,
        }
    }
}

/// A fuzzy set defined over a universe of discourse
#[derive(Debug, Clone)]
pub struct FuzzySet {
    /// Name of the fuzzy set (e.g., "hot", "cold")
    pub name: String,
    /// The membership function
    pub membership: MembershipFunction,
    /// Universe bounds
    pub universe: (f64, f64),
}

impl FuzzySet {
    pub fn new(name: impl Into<String>, membership: MembershipFunction, universe: (f64, f64)) -> Self {
        Self {
            name: name.into(),
            membership,
            universe,
        }
    }

    /// Get membership degree for a value
    pub fn membership_of(&self, x: f64) -> FuzzyValue {
        self.membership.evaluate(x)
    }

    /// Alpha-cut: return interval where membership >= alpha
    pub fn alpha_cut(&self, alpha: f64, samples: usize) -> Option<(f64, f64)> {
        let step = (self.universe.1 - self.universe.0) / (samples as f64);
        let mut left = None;
        let mut right = None;

        for i in 0..=samples {
            let x = self.universe.0 + (i as f64) * step;
            if self.membership_of(x).value() >= alpha {
                if left.is_none() {
                    left = Some(x);
                }
                right = Some(x);
            }
        }

        match (left, right) {
            (Some(l), Some(r)) => Some((l, r)),
            _ => None,
        }
    }
}

/// A linguistic variable with associated fuzzy sets
#[derive(Debug, Clone)]
pub struct LinguisticVariable {
    /// Variable name (e.g., "temperature")
    pub name: String,
    /// Universe of discourse
    pub universe: (f64, f64),
    /// Fuzzy sets (terms) for this variable
    pub terms: HashMap<String, FuzzySet>,
}

impl LinguisticVariable {
    pub fn new(name: impl Into<String>, universe: (f64, f64)) -> Self {
        Self {
            name: name.into(),
            universe,
            terms: HashMap::new(),
        }
    }

    /// Add a term (fuzzy set) to this variable
    pub fn add_term(&mut self, name: impl Into<String>, membership: MembershipFunction) {
        let term_name = name.into();
        let set = FuzzySet::new(term_name.clone(), membership, self.universe);
        self.terms.insert(term_name, set);
    }

    /// Fuzzify a crisp value - get membership for all terms
    pub fn fuzzify(&self, value: f64) -> HashMap<String, FuzzyValue> {
        self.terms
            .iter()
            .map(|(name, set)| (name.clone(), set.membership_of(value)))
            .collect()
    }

    /// Get the term with highest membership for a value
    pub fn dominant_term(&self, value: f64) -> Option<(&str, FuzzyValue)> {
        self.terms
            .iter()
            .map(|(name, set)| (name.as_str(), set.membership_of(value)))
            .max_by(|a, b| a.1.value().partial_cmp(&b.1.value()).unwrap())
    }
}

/// Inference method for fuzzy rules
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum InferenceMethod {
    /// Mamdani inference (min implication, max aggregation)
    Mamdani,
    /// Larsen inference (product implication, max aggregation)
    Larsen,
    /// Sugeno inference (weighted average)
    Sugeno,
}

/// Defuzzification method
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum DefuzzificationMethod {
    /// Center of Gravity (Centroid)
    Centroid,
    /// Bisector of Area
    Bisector,
    /// Mean of Maximum
    MeanOfMaximum,
    /// Smallest of Maximum
    SmallestOfMaximum,
    /// Largest of Maximum
    LargestOfMaximum,
    /// Weighted Average (for Sugeno)
    WeightedAverage,
}

/// A fuzzy rule antecedent (condition)
#[derive(Debug, Clone)]
pub struct FuzzyAntecedent {
    /// Variable name
    pub variable: String,
    /// Term name (fuzzy set)
    pub term: String,
    /// Hedges applied (e.g., "very", "somewhat")
    pub hedges: Vec<Hedge>,
}

/// Linguistic hedges
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum Hedge {
    /// Concentration (square)
    Very,
    /// Dilation (square root)
    Somewhat,
    /// Intensification
    Extremely,
    /// Approximate
    More,
    /// Approximate
    Less,
    /// Negation
    Not,
}

impl Hedge {
    pub fn apply(&self, value: FuzzyValue) -> FuzzyValue {
        let v = value.value();
        match self {
            Hedge::Very => FuzzyValue::new(v * v),
            Hedge::Somewhat => FuzzyValue::new(v.sqrt()),
            Hedge::Extremely => FuzzyValue::new(v * v * v),
            Hedge::More => FuzzyValue::new(v.powf(1.25)),
            Hedge::Less => FuzzyValue::new(v.powf(0.75)),
            Hedge::Not => value.not(),
        }
    }
}

/// A fuzzy rule consequent (conclusion)
#[derive(Debug, Clone)]
pub struct FuzzyConsequent {
    /// Variable name
    pub variable: String,
    /// Term name (fuzzy set) for Mamdani/Larsen
    pub term: Option<String>,
    /// Singleton value for Sugeno
    pub singleton: Option<f64>,
    /// Coefficients for Sugeno (linear function of inputs)
    pub coefficients: Option<Vec<(String, f64)>>,
}

/// A fuzzy rule
#[derive(Debug, Clone)]
pub struct FuzzyRule {
    /// Rule name/label
    pub name: Option<String>,
    /// Antecedents (ANDed together)
    pub antecedents: Vec<FuzzyAntecedent>,
    /// Consequent
    pub consequent: FuzzyConsequent,
    /// Rule weight
    pub weight: f64,
}

impl FuzzyRule {
    pub fn new(antecedents: Vec<FuzzyAntecedent>, consequent: FuzzyConsequent) -> Self {
        Self {
            name: None,
            antecedents,
            consequent,
            weight: 1.0,
        }
    }

    pub fn with_weight(mut self, weight: f64) -> Self {
        self.weight = weight;
        self
    }

    pub fn with_name(mut self, name: impl Into<String>) -> Self {
        self.name = Some(name.into());
        self
    }
}

/// Fuzzy Inference System
pub struct FuzzyInferenceSystem {
    /// Input linguistic variables
    pub inputs: HashMap<String, LinguisticVariable>,
    /// Output linguistic variables
    pub outputs: HashMap<String, LinguisticVariable>,
    /// Fuzzy rules
    pub rules: Vec<FuzzyRule>,
    /// Inference method
    pub inference_method: InferenceMethod,
    /// Defuzzification method
    pub defuzz_method: DefuzzificationMethod,
    /// Number of samples for defuzzification
    pub samples: usize,
}

impl FuzzyInferenceSystem {
    pub fn new() -> Self {
        Self {
            inputs: HashMap::new(),
            outputs: HashMap::new(),
            rules: Vec::new(),
            inference_method: InferenceMethod::Mamdani,
            defuzz_method: DefuzzificationMethod::Centroid,
            samples: 100,
        }
    }

    /// Add an input variable
    pub fn add_input(&mut self, var: LinguisticVariable) {
        self.inputs.insert(var.name.clone(), var);
    }

    /// Add an output variable
    pub fn add_output(&mut self, var: LinguisticVariable) {
        self.outputs.insert(var.name.clone(), var);
    }

    /// Add a rule
    pub fn add_rule(&mut self, rule: FuzzyRule) {
        self.rules.push(rule);
    }

    /// Evaluate the fuzzy inference system
    pub fn evaluate(&self, inputs: &HashMap<String, f64>) -> HashMap<String, f64> {
        match self.inference_method {
            InferenceMethod::Mamdani | InferenceMethod::Larsen => {
                self.evaluate_mamdani_larsen(inputs)
            }
            InferenceMethod::Sugeno => self.evaluate_sugeno(inputs),
        }
    }

    /// Mamdani/Larsen evaluation
    fn evaluate_mamdani_larsen(&self, inputs: &HashMap<String, f64>) -> HashMap<String, f64> {
        let mut output_aggregates: HashMap<String, Vec<(f64, FuzzyValue)>> = HashMap::new();

        // Initialize output aggregates
        for (name, var) in &self.outputs {
            let step = (var.universe.1 - var.universe.0) / (self.samples as f64);
            let points: Vec<(f64, FuzzyValue)> = (0..=self.samples)
                .map(|i| (var.universe.0 + (i as f64) * step, FuzzyValue::new(0.0)))
                .collect();
            output_aggregates.insert(name.clone(), points);
        }

        // Evaluate each rule
        for rule in &self.rules {
            // Compute firing strength (AND of all antecedents)
            let mut firing_strength = FuzzyValue::new(1.0);

            for antecedent in &rule.antecedents {
                if let Some(input_var) = self.inputs.get(&antecedent.variable) {
                    if let Some(input_value) = inputs.get(&antecedent.variable) {
                        if let Some(term) = input_var.terms.get(&antecedent.term) {
                            let mut membership = term.membership_of(*input_value);

                            // Apply hedges
                            for hedge in &antecedent.hedges {
                                membership = hedge.apply(membership);
                            }

                            firing_strength = firing_strength.and(&membership);
                        }
                    }
                }
            }

            // Apply rule weight
            firing_strength = FuzzyValue::new(firing_strength.value() * rule.weight);

            // Apply implication to consequent
            if let Some(output_var) = self.outputs.get(&rule.consequent.variable) {
                if let Some(term_name) = &rule.consequent.term {
                    if let Some(term) = output_var.terms.get(term_name) {
                        if let Some(aggregate) = output_aggregates.get_mut(&rule.consequent.variable) {
                            for (x, current) in aggregate.iter_mut() {
                                let term_value = term.membership_of(*x);
                                let implied = match self.inference_method {
                                    InferenceMethod::Mamdani => {
                                        firing_strength.implies_mamdani(&term_value)
                                    }
                                    InferenceMethod::Larsen => {
                                        firing_strength.implies_larsen(&term_value)
                                    }
                                    _ => firing_strength,
                                };
                                *current = current.or(&implied);
                            }
                        }
                    }
                }
            }
        }

        // Defuzzify each output
        let mut results = HashMap::new();
        for (name, aggregate) in &output_aggregates {
            let crisp = self.defuzzify(aggregate);
            results.insert(name.clone(), crisp);
        }

        results
    }

    /// Sugeno evaluation
    fn evaluate_sugeno(&self, inputs: &HashMap<String, f64>) -> HashMap<String, f64> {
        let mut output_sums: HashMap<String, f64> = HashMap::new();
        let mut weight_sums: HashMap<String, f64> = HashMap::new();

        for rule in &self.rules {
            let mut firing_strength = FuzzyValue::new(1.0);

            for antecedent in &rule.antecedents {
                if let Some(input_var) = self.inputs.get(&antecedent.variable) {
                    if let Some(input_value) = inputs.get(&antecedent.variable) {
                        if let Some(term) = input_var.terms.get(&antecedent.term) {
                            let mut membership = term.membership_of(*input_value);

                            for hedge in &antecedent.hedges {
                                membership = hedge.apply(membership);
                            }

                            firing_strength = firing_strength.and(&membership);
                        }
                    }
                }
            }

            let weight = firing_strength.value() * rule.weight;

            let consequent_value = if let Some(singleton) = rule.consequent.singleton {
                singleton
            } else if let Some(ref coeffs) = rule.consequent.coefficients {
                let mut value = 0.0;
                for (var_name, coeff) in coeffs {
                    if var_name == "_constant" {
                        value += coeff;
                    } else if let Some(input_value) = inputs.get(var_name) {
                        value += coeff * input_value;
                    }
                }
                value
            } else {
                continue;
            };

            let output_name = &rule.consequent.variable;
            *output_sums.entry(output_name.clone()).or_insert(0.0) += weight * consequent_value;
            *weight_sums.entry(output_name.clone()).or_insert(0.0) += weight;
        }

        let mut results = HashMap::new();
        for (name, sum) in &output_sums {
            let weight_sum = weight_sums.get(name).copied().unwrap_or(1.0);
            if weight_sum > 0.0 {
                results.insert(name.clone(), sum / weight_sum);
            } else if let Some(var) = self.outputs.get(name) {
                results.insert(name.clone(), (var.universe.0 + var.universe.1) / 2.0);
            }
        }

        results
    }

    /// Defuzzify an aggregate fuzzy set
    fn defuzzify(&self, aggregate: &[(f64, FuzzyValue)]) -> f64 {
        match self.defuzz_method {
            DefuzzificationMethod::Centroid => {
                let mut numerator = 0.0;
                let mut denominator = 0.0;
                for (x, m) in aggregate {
                    numerator += x * m.value();
                    denominator += m.value();
                }
                if denominator > 0.0 {
                    numerator / denominator
                } else {
                    (aggregate.first().map(|p| p.0).unwrap_or(0.0)
                        + aggregate.last().map(|p| p.0).unwrap_or(0.0))
                        / 2.0
                }
            }
            DefuzzificationMethod::Bisector => {
                let total_area: f64 = aggregate.iter().map(|(_, m)| m.value()).sum();
                let half_area = total_area / 2.0;
                let mut cumulative = 0.0;
                for (x, m) in aggregate {
                    cumulative += m.value();
                    if cumulative >= half_area {
                        return *x;
                    }
                }
                aggregate.last().map(|p| p.0).unwrap_or(0.0)
            }
            DefuzzificationMethod::MeanOfMaximum => {
                let max_value = aggregate
                    .iter()
                    .map(|(_, m)| m.value())
                    .fold(0.0, f64::max);
                let maxima: Vec<f64> = aggregate
                    .iter()
                    .filter(|(_, m)| (m.value() - max_value).abs() < f64::EPSILON)
                    .map(|(x, _)| *x)
                    .collect();
                if maxima.is_empty() {
                    (aggregate.first().map(|p| p.0).unwrap_or(0.0)
                        + aggregate.last().map(|p| p.0).unwrap_or(0.0))
                        / 2.0
                } else {
                    maxima.iter().sum::<f64>() / maxima.len() as f64
                }
            }
            DefuzzificationMethod::SmallestOfMaximum => {
                let max_value = aggregate
                    .iter()
                    .map(|(_, m)| m.value())
                    .fold(0.0, f64::max);
                aggregate
                    .iter()
                    .find(|(_, m)| (m.value() - max_value).abs() < f64::EPSILON)
                    .map(|(x, _)| *x)
                    .unwrap_or(0.0)
            }
            DefuzzificationMethod::LargestOfMaximum => {
                let max_value = aggregate
                    .iter()
                    .map(|(_, m)| m.value())
                    .fold(0.0, f64::max);
                aggregate
                    .iter()
                    .rev()
                    .find(|(_, m)| (m.value() - max_value).abs() < f64::EPSILON)
                    .map(|(x, _)| *x)
                    .unwrap_or(0.0)
            }
            DefuzzificationMethod::WeightedAverage => {
                let mut numerator = 0.0;
                let mut denominator = 0.0;
                for (x, m) in aggregate {
                    numerator += x * m.value();
                    denominator += m.value();
                }
                if denominator > 0.0 { numerator / denominator } else { 0.0 }
            }
        }
    }
}

impl Default for FuzzyInferenceSystem {
    fn default() -> Self {
        Self::new()
    }
}

/// Fuzzy triple with degree of membership
#[derive(Debug, Clone)]
pub struct FuzzyTriple {
    /// The triple
    pub triple: Triple,
    /// Degree of truth
    pub degree: FuzzyValue,
}

impl FuzzyTriple {
    pub fn new(triple: Triple, degree: FuzzyValue) -> Self {
        Self { triple, degree }
    }
}

/// T-norm types
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TNorm {
    Min,
    Product,
    Lukasiewicz,
    Drastic,
}

impl TNorm {
    pub fn apply(&self, a: FuzzyValue, b: FuzzyValue) -> FuzzyValue {
        match self {
            TNorm::Min => a.and(&b),
            TNorm::Product => a.product(&b),
            TNorm::Lukasiewicz => a.lukasiewicz_and(&b),
            TNorm::Drastic => {
                if a.value() == 1.0 { b }
                else if b.value() == 1.0 { a }
                else { FuzzyValue::new(0.0) }
            }
        }
    }
}

/// T-conorm types
#[derive(Debug, Clone, Copy, PartialEq)]
pub enum TConorm {
    Max,
    ProbabilisticSum,
    BoundedSum,
    Drastic,
}

impl TConorm {
    pub fn apply(&self, a: FuzzyValue, b: FuzzyValue) -> FuzzyValue {
        match self {
            TConorm::Max => a.or(&b),
            TConorm::ProbabilisticSum => {
                FuzzyValue::new(a.value() + b.value() - a.value() * b.value())
            }
            TConorm::BoundedSum => a.bounded_sum(&b),
            TConorm::Drastic => {
                if a.value() == 0.0 { b }
                else if b.value() == 0.0 { a }
                else { FuzzyValue::new(1.0) }
            }
        }
    }
}

/// Fuzzy N3 Reasoner that integrates fuzzy logic with N3 rules
pub struct FuzzyN3Reasoner {
    /// Fuzzy triples in the store
    triples: Vec<FuzzyTriple>,
    /// Linguistic variables for fuzzification
    variables: HashMap<String, LinguisticVariable>,
    /// T-norm to use for conjunction
    t_norm: TNorm,
    /// T-conorm to use for disjunction
    t_conorm: TConorm,
    /// Threshold for considering a triple "true"
    truth_threshold: f64,
}

impl FuzzyN3Reasoner {
    pub fn new() -> Self {
        Self {
            triples: Vec::new(),
            variables: HashMap::new(),
            t_norm: TNorm::Min,
            t_conorm: TConorm::Max,
            truth_threshold: 0.5,
        }
    }

    /// Set the T-norm for conjunction
    pub fn set_t_norm(&mut self, t_norm: TNorm) {
        self.t_norm = t_norm;
    }

    /// Set the T-conorm for disjunction
    pub fn set_t_conorm(&mut self, t_conorm: TConorm) {
        self.t_conorm = t_conorm;
    }

    /// Set truth threshold
    pub fn set_threshold(&mut self, threshold: f64) {
        self.truth_threshold = threshold;
    }

    /// Add a fuzzy triple
    pub fn add_triple(&mut self, triple: Triple, degree: FuzzyValue) {
        self.triples.push(FuzzyTriple::new(triple, degree));
    }

    /// Add a crisp triple (degree = 1.0)
    pub fn add_crisp_triple(&mut self, triple: Triple) {
        self.add_triple(triple, FuzzyValue::new(1.0));
    }

    /// Add a linguistic variable
    pub fn add_variable(&mut self, var: LinguisticVariable) {
        self.variables.insert(var.name.clone(), var);
    }

    /// Get "true" triples (above threshold)
    pub fn true_triples(&self) -> Vec<&FuzzyTriple> {
        self.triples
            .iter()
            .filter(|ft| ft.degree.is_true(self.truth_threshold))
            .collect()
    }

    /// Get all triples
    pub fn all_triples(&self) -> &[FuzzyTriple] {
        &self.triples
    }
}

impl Default for FuzzyN3Reasoner {
    fn default() -> Self {
        Self::new()
    }
}

#[cfg(test)]
mod tests {
    use super::*;

    #[test]
    fn test_fuzzy_value_operations() {
        let a = FuzzyValue::new(0.6);
        let b = FuzzyValue::new(0.4);

        assert!((a.and(&b).value() - 0.4).abs() < 0.001);
        assert!((a.or(&b).value() - 0.6).abs() < 0.001);
        assert!((a.not().value() - 0.4).abs() < 0.001);
        assert!((a.product(&b).value() - 0.24).abs() < 0.001);
    }

    #[test]
    fn test_triangular_membership() {
        let mf = MembershipFunction::Triangular(0.0, 5.0, 10.0);

        assert!((mf.evaluate(0.0).value() - 0.0).abs() < 0.001);
        assert!((mf.evaluate(5.0).value() - 1.0).abs() < 0.001);
        assert!((mf.evaluate(10.0).value() - 0.0).abs() < 0.001);
        assert!((mf.evaluate(2.5).value() - 0.5).abs() < 0.001);
        assert!((mf.evaluate(7.5).value() - 0.5).abs() < 0.001);
    }

    #[test]
    fn test_gaussian_membership() {
        let mf = MembershipFunction::Gaussian(5.0, 1.0);

        assert!((mf.evaluate(5.0).value() - 1.0).abs() < 0.001);
        assert!(mf.evaluate(6.0).value() < 1.0);
        assert!(mf.evaluate(6.0).value() > 0.5);
    }

    #[test]
    fn test_linguistic_variable() {
        let mut temp = LinguisticVariable::new("temperature", (0.0, 100.0));
        temp.add_term("cold", MembershipFunction::Trapezoidal(0.0, 0.0, 20.0, 40.0));
        temp.add_term("warm", MembershipFunction::Triangular(30.0, 50.0, 70.0));
        temp.add_term("hot", MembershipFunction::Trapezoidal(60.0, 80.0, 100.0, 100.0));

        let fuzzified = temp.fuzzify(35.0);

        assert!(fuzzified.get("cold").unwrap().value() > 0.0);
        assert!(fuzzified.get("warm").unwrap().value() > 0.0);
        assert!((fuzzified.get("hot").unwrap().value() - 0.0).abs() < 0.001);
    }

    #[test]
    fn test_hedges() {
        let value = FuzzyValue::new(0.5);

        let very = Hedge::Very.apply(value);
        assert!((very.value() - 0.25).abs() < 0.001);

        let somewhat = Hedge::Somewhat.apply(value);
        assert!((somewhat.value() - 0.7071).abs() < 0.01);

        let not = Hedge::Not.apply(value);
        assert!((not.value() - 0.5).abs() < 0.001);
    }

    #[test]
    fn test_t_norms() {
        let a = FuzzyValue::new(0.6);
        let b = FuzzyValue::new(0.4);

        assert!((TNorm::Min.apply(a, b).value() - 0.4).abs() < 0.001);
        assert!((TNorm::Product.apply(a, b).value() - 0.24).abs() < 0.001);
        assert!((TNorm::Lukasiewicz.apply(a, b).value() - 0.0).abs() < 0.001);
    }

    #[test]
    fn test_t_conorms() {
        let a = FuzzyValue::new(0.6);
        let b = FuzzyValue::new(0.4);

        assert!((TConorm::Max.apply(a, b).value() - 0.6).abs() < 0.001);
        assert!((TConorm::ProbabilisticSum.apply(a, b).value() - 0.76).abs() < 0.001);
        assert!((TConorm::BoundedSum.apply(a, b).value() - 1.0).abs() < 0.001);
    }
}
