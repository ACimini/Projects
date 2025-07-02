import numpy as np
from typing import NamedTuple, Optional
from pandas import Timestamp
import matplotlib.pyplot as plt
import matplotlib.axes as axes
from matplotlib.patches import Rectangle
from scipy.optimize import least_squares
import math
from utils import all_are_some, none_are_some
from sklearn.model_selection import train_test_split



class ClientRow(NamedTuple):
    Index: int
    client_identifier: int
    industry_vertical: str
    comments: Optional[str]
    broker_changeability: Optional[str]
    client_count: float
    renewal_day: float
    renewal_month: float
    renewal_year: float
    upcoming_renewal: Timestamp
    modified: Timestamp
    modified_by: str
    region: str
    industry_description: str
    complex_tier: Optional[str]
    office: str
    strategic_advisor: Optional[str]
    broker: Optional[str]
    sx_broker: str
    sab: str
    ab: Optional[str]
    analyst: Optional[str]
    producer: Optional[str]
    client_advocate: Optional[str]
    revenue_to_wtw: float
    revenue_type: Optional[str]
    pe_owned: Optional[bool]
    pe_company: Optional[str]
    unbundled_tpa: Optional[str]
    m_and_a_activity_four_plus_annually: Optional[str]
    renewal_qtr: str
    sic_code: float | str
    epic_code: str
    current_total_collateral: Optional[float]
    wc_carrier: Optional[str]
    wc_deductible: Optional[float]
    wc_premium: Optional[float]
    wc_loss_pick_at_retention: Optional[float]
    auto_carrier: Optional[str]
    auto_deductible: Optional[float]
    auto_premium: Optional[float]
    auto_loss_pick_at_retention: Optional[float]
    fleet_count: Optional[int]
    gl_carrier: Optional[str]
    gl_deductible: Optional[float]
    gl_premium: Optional[float]
    gl_loss_pick_at_retention: Optional[float]
    total_primary_premium: float
    buffer_carrier: Optional[str]
    buffer_limit: Optional[float]
    buffer_premium: Optional[float]
    umbrella_carrier: str
    umbrella_limit: float
    lead_premium: float
    access_point: Optional[str]
    total_limit: float
    gl_attachment: Optional[float]
    al_attachment: Optional[float]
    xs_layer_1_carrier: str
    xs_layer_1_limit: float
    xs_layer_1_attachment: float
    xs_layer_1_premium: float
    xs_layer_2_carrier: str
    xs_layer_2_limit: float
    xs_layer_2_attachment: float
    xs_layer_2_premium: float
    xs_layer_3_carrier: str
    xs_layer_3_limit: float
    xs_layer_3_attachment: float
    xs_layer_3_premium: float
    xs_layer_4_carrier: Optional[str]
    xs_layer_4_limit: Optional[float]
    xs_layer_4_attachment: Optional[float]
    xs_layer_4_premium: Optional[float]
    xs_layer_5_carrier: Optional[str]
    xs_layer_5_limit: Optional[float]
    xs_layer_5_attachment: Optional[float]
    xs_layer_5_premium: Optional[float]
    xs_layer_6_carrier: Optional[str]
    xs_layer_6_limit: Optional[float]
    xs_layer_6_attachment: Optional[float]
    xs_layer_6_premium: Optional[float]
    xs_layer_7_carrier: Optional[str]
    xs_layer_7_limit: Optional[float]
    xs_layer_7_attachment: Optional[float]
    xs_layer_7_premium: Optional[float]
    xs_layer_8_carrier: Optional[str]
    xs_layer_8_limit: Optional[float]
    xs_layer_8_attachment: Optional[float]
    xs_layer_8_premium: Optional[float]
    xs_layer_9_carrier: Optional[str]
    xs_layer_9_limit: Optional[float]
    xs_layer_9_attachment: Optional[float]
    xs_layer_9_premium: Optional[float]
    xs_layer_10_carrier: Optional[str]
    xs_layer_10_limit: Optional[float]
    xs_layer_10_attachment: Optional[float]
    xs_layer_10_premium: Optional[float]
    xs_layer_11_carrier: Optional[str]
    xs_layer_11_limit: Optional[float]
    xs_layer_11_attachment: Optional[float]
    xs_layer_11_premium: Optional[float]
    xs_layer_12_carrier: Optional[str]
    xs_layer_12_limit: Optional[float]
    xs_layer_12_attachment: Optional[float]
    xs_layer_12_premium: Optional[float]
    xs_layer_13_carrier: Optional[str]
    xs_layer_13_limit: Optional[float]
    xs_layer_13_attachment: Optional[float]
    xs_layer_13_premium: Optional[float]
    xs_layer_14_carrier: Optional[str]
    xs_layer_14_limit: Optional[float]
    xs_layer_14_attachment: Optional[float]
    xs_layer_14_premium: Optional[float]
    xs_layer_15_carrier: Optional[str]
    xs_layer_15_limit: Optional[float]
    xs_layer_15_attachment: Optional[float]
    xs_layer_15_premium: Optional[float]
    xs_layer_16_carrier: Optional[str]
    xs_layer_16_limit: Optional[float]
    xs_layer_16_attachment: Optional[float]
    xs_layer_16_premium: Optional[float]
    xs_layer_17_carrier: Optional[str]
    xs_layer_17_limit: Optional[float]
    xs_layer_17_attachment: Optional[float]
    xs_layer_17_premium: Optional[float]
    xs_layer_18_carrier: Optional[str]
    xs_layer_18_limit: Optional[float]
    xs_layer_18_attachment: Optional[float]
    xs_layer_18_premium: Optional[float]
    xs_layer_19_carrier: Optional[str]
    xs_layer_19_limit: Optional[float]
    xs_layer_19_attachment: Optional[float]
    xs_layer_19_premium: Optional[float]
    xs_layer_20_carrier: Optional[str]
    xs_layer_20_limit: Optional[float]
    xs_layer_20_attachment: Optional[float]
    xs_layer_20_premium: Optional[float]
    today_date: Timestamp
    client_logo: Optional[str]
    id: int
    item_type: str
    path: str



MALFORMED_ROWS = (
    5,
    9,
    14,
    26,
    27,
    29,
    30,
    31,
    47,
    48,
    51,
    66,
    69,
    72,
    74,
    79,
    85,
    86,
    93,
    95,
    98,
    102,
    106,
    108,
    110,
    112,
    115,
    119,
    126,
    128,
    134,
    135,
    146,
    148,
    151,
    154,
    161,
    164,
    166,
    167,
    173,
    176,
    177,
    178,
    180,
    184,
    190,
    196,
    208,
    218,
    222,
    226,
    228,
    230,
    231,
    234,
    235,
    236,
    239,
    243,
    246,
    247,
    248,
    252,
    253,
    262,
    263,
    276,
    279,
    281,
    282,
    284,
    285,
    286,
    287,
    288,
    289,
    294,
    296,
    297,
    303,
    307,
    314,
    315,
    319,
    322,
    332,
    334,
    339,
    342,
    344,
    345,
    349,
    350,
    352,
    358,
    360,
    367,
    368,
    370,
    373,
    388,
    389,
    391,
    393,
    394,
    396,
    403,
    404,
    405,
    408,
    410,
    413,
    416,
    419,
    422,
    424,
    425,
    427,
    428,
    432,
    434,
    435,
    436,
    437,
    438,
    439,
    442,
    446,
    453,
    454,
    457,
    459,
    467,
    470,
    474,
    476,
    479,
    483,
    485,
    486,
    487,
    489,
    491,
    494,
    495,
    498,
    500,
    511,
    512,
    518,
    521,
    526,
    527,
    539,
    550,
    551,
    555,
    556,
    557
)

EMPTY_ROWS = (
    3,
    4,
    6,
    7,
    8,
    10,
    11,
    12,
    15,
    16,
    17,
    18,
    19,
    20,
    21,
    22,
    23,
    24,
    25,
    32,
    33,
    34,
    35,
    36,
    38,
    39,
    40,
    41,
    42,
    43,
    44,
    45,
    49,
    50,
    54,
    55,
    91,
    96,
    104,
    105,
    107,
    111,
    125,
    127,
    129,
    130,
    133,
    145,
    152,
    153,
    155,
    156,
    157,
    165,
    169,
    170,
    182,
    183,
    191,
    197,
    198,
    199,
    200,
    201,
    202,
    203,
    204,
    205,
    206,
    207,
    209,
    210,
    211,
    212,
    213,
    214,
    215,
    216,
    217,
    219,
    221,
    223,
    224,
    225,
    227,
    229,
    249,
    251,
    264,
    266,
    269,
    280,
    290,
    292,
    295,
    298,
    299,
    300,
    301,
    304,
    305,
    306,
    316,
    317,
    324,
    327,
    331,
    333,
    335,
    336,
    347,
    354,
    357,
    362,
    363,
    364,
    372,
    374,
    379,
    380,
    384,
    386,
    387,
    390,
    392,
    411,
    412,
    415,
    423,
    426,
    429,
    430,
    440,
    444,
    445,
    447,
    448,
    455,
    456,
    460,
    461,
    463,
    464,
    465,
    466,
    469,
    472,
    473,
    475,
    480,
    481,
    484,
    490,
    493,
    496,
    497,
    499,
    502,
    503,
    504,
    505,
    506,
    507,
    508,
    509,
    510,
    513,
    514,
    515,
    516,
    519,
    520,
    522,
    524,
    525,
    530,
    531,
    532,
    537,
    538,
    540,
    541,
    544,
    545,
    546,
    547,
    548,
    549,
    552,
    553,
    554,
)

APPROVED_RATE_INVERSIONS = (
    64,
    67,
    81,
    101,
    124,
    261,
    395,
)



class SicCode:
    AGRICULTURE = 'Agriculture, Forestry and Fishing'
    MINING = 'Mining'
    CONSTRUCTION = 'Construction'
    MANUFACTURING = 'Manufacturing'
    TRANS_COMM = 'Transportation, Communications, Electric, Gas and Sanitary service'
    WHOLESALE = 'Wholesale Trade'
    RETAIL = 'Retail Trade'
    FINANCE_INSURANCE = 'Finance, Insurance and Real estate'
    SERVICES = 'Services'
    PUBLIC_ADMIN = 'Public administration'
    NON_CLASSIFIABLE = 'Nonclassifiable'
    INDUSTRY_NAMES = (
        AGRICULTURE,
        MINING,
        CONSTRUCTION,
        MANUFACTURING,
        TRANS_COMM,
        WHOLESALE,
        RETAIL,
        FINANCE_INSURANCE,
        SERVICES,
        PUBLIC_ADMIN,
        NON_CLASSIFIABLE
    )

    def __init__(self, code: float):
        assert type(code) is float, f'Provided SIC code "{code}" is not a float'
        assert not np.isnan(code), 'Provided SIC Code was nan'
        assert round(code) == code, 'Provided SIC Code is not a likely integer'
        self.__code = int(code)
    def __str__(self):
        return f'SIC({self.__code})'

    #https://en.wikipedia.org/wiki/Standard_Industrial_Classification#Range
    def get_major_industry_name(self) -> str:
        return SicCode.INDUSTRY_NAMES[self.get_major_industry_id()]
    def get_major_industry_id(self) -> int:
        if self.__code < 100:
            raise Exception(f'SIC {self.__code} is not a valid code')
        elif self.__code <= 999:
            return 0
        elif self.__code <= 1499:
            return 1
        elif self.__code <= 1799:
            return 2
        elif self.__code <= 1999:
            raise Exception(f'SIC {self.__code} is not a valid code')
        elif self.__code <= 3999:
            return 3
        elif self.__code <= 4999:
            return 4
        elif self.__code <= 5199:
            return 5
        elif self.__code <= 5999:
            return 6
        elif self.__code <= 6799:
            return 7
        elif self.__code <= 6999:
            raise Exception(f'SIC {self.__code} is not a valid code')
        elif self.__code <= 8999:
            return 8
        elif self.__code <= 9729:
            return 9
        elif self.__code <= 9899:
            raise Exception(f'SIC {self.__code} is not a valid code')
        elif self.__code <= 9999:
            return 10
        else:
            raise Exception(f'SIC {self.__code} is not a valid code')
    def get_major_industry(self) -> int:
        return self.__code // 100
    def get_sub_classification(self) -> int:
        return self.__code // 10
    def get_full_code(self) -> int:
        return self.__code
DEFAULT_SIC_CODE = SicCode(9999.0)



class Policy:
    def __init__(self, carrier: str, premium: int):
        self.carrier = carrier or 'Placeholder'
        assert not np.isnan(premium), f'carrier "{carrier}" premium is nan'
        assert type(premium) is int or type(premium) is float, f'carrier "{carrier}" premium of "{premium}" is string'
        assert premium > 0, f'carrier "{carrier}" premium is <=0'
        self.premium = premium
    def __str__(self):
        return f'{self.carrier}: for ${self.premium}'

class DeductiblePolicy(Policy):
    def __init__(self, carrier, premium, deductible: int, loss_pick_at_retention: int):
        super().__init__(carrier, premium)
        self.deductible = deductible
        self.loss_pick_at_retention = loss_pick_at_retention

class AutoPolicy(DeductiblePolicy):
    def __init__(self, carrier, premium, deductible: int, loss_pick_at_retention: int, fleet_count: int):
        super().__init__(carrier, premium, deductible, loss_pick_at_retention)
        self.fleet_count = fleet_count

class LayerPolicy(Policy):
    def __init__(self, layer: int, carrier: str, premium: int, limit: int, attachment: int):
        super().__init__(carrier, premium)
        self.layer = layer
        self.limit = limit
        self.attachment = attachment
    def __str__(self):
        return f'{self.carrier} for {self.limit/1_000_000}M xs {self.attachment/1_000_000}M at layer {self.layer} for ${self.premium} (${self.per_mil_prem()} per mil)'

    def get_shares(self) -> tuple['LayerPolicy']:
        return (self, )
    def get_carrier_str(self) -> str:
        return self.carrier
    def per_mil_prem(self) -> float:
        return self.premium / (self.limit / 1_000_000)
    def get_carrier_count(self) -> int:
        return 1

class QuotaShare:
    def __init__(self, first_share: LayerPolicy):
        self.__shares = [first_share]
        self.layer = first_share.layer
        self.attachment = first_share.attachment
        self.limit = first_share.limit
        self.premium = first_share.premium
    def __str__(self):
        return f'Quota share with {len(self.__shares)} policies for {self.limit/1_000_000}M xs {self.attachment/1_000_000}M at layer {self.layer} for ${self.premium} (${self.per_mil_prem()} per mil)'

    def __add_policy(self, policy: LayerPolicy):
        assert policy.attachment == self.__shares[0].attachment
        self.__shares.append(policy)
        self.limit += policy.limit
        self.premium += policy.premium

    @staticmethod
    def append_or_create(others: 'QuotaShare | LayerPolicy', added_policy: LayerPolicy) -> 'QuotaShare':
        if type(others) is LayerPolicy:
            others = QuotaShare(others)
        others.__add_policy(added_policy)
        return others

    def get_shares(self) -> tuple[LayerPolicy]:
        return tuple(self.__shares)
    def get_carrier_str(self) -> str:
        ', '.join(map(lambda x: x.carrier, self.__shares))
    def per_mil_prem(self) -> float:
        return self.premium / (self.limit / 1_000_000)
    def get_carrier_count(self) -> int:
        return len(self.__shares)

class ExcessPolicy:
    INSTANTANEOUS = lambda a, b, theta, x: a * theta ** x + b
    ANTIDERIVATIVE = lambda a, b, theta, x: a / math.log(theta) * theta ** x + b * x
    AREA_BETWEEN = lambda a, b, theta, p1, p2: ExcessPolicy.ANTIDERIVATIVE(a, b, theta, p2) - ExcessPolicy.ANTIDERIVATIVE(a, b, theta, p1)

    ALMOST_ZERO = 1e-10
    ALMOST_ONE = 1 - 1e-10

    def __init__(self, row: ClientRow):
        #Add lead umbrella layer
        casualty_layers: list[LayerPolicy | QuotaShare] = []
        if all_are_some([row.umbrella_carrier, row.umbrella_limit, row.lead_premium]):
            casualty_layers.append(LayerPolicy(0, row.umbrella_carrier, row.lead_premium, row.umbrella_limit, 0))

        if len(casualty_layers) == 0 and not none_are_some([row.umbrella_carrier, row.umbrella_limit, row.lead_premium]):
            raise Exception(f'Index {row.Index+1} might have umbrella {[row.umbrella_carrier, row.umbrella_limit, row.lead_premium]}')

        #Build layers of tower
        layer = 1
        was_prev_present = len(casualty_layers) == 1
        for i in range(1, 21):
            carrier = getattr(row, f'xs_layer_{i}_carrier')
            limit = getattr(row, f'xs_layer_{i}_limit')
            premium = getattr(row, f'xs_layer_{i}_premium')
            attachment = getattr(row, f'xs_layer_{i}_attachment')
            if all_are_some([carrier, limit, premium, attachment]):
                if not was_prev_present:
                    raise Exception(f'Missing layer at id {row.Index+1} while adding layer {i}/{layer}')
                policy = LayerPolicy(layer, carrier, premium, limit, attachment)
                if len(casualty_layers) > 0 and policy.attachment == casualty_layers[-1].attachment:
                    last_added = casualty_layers.pop()
                    casualty_layers.append(QuotaShare.append_or_create(last_added, policy))
                else:
                    casualty_layers.append(policy)
                    layer += 1
                was_prev_present = True
            elif not none_are_some([carrier, limit, premium, attachment]):
                raise Exception(f'Partial layer at id {row.Index+1} while adding layer {i}/{layer}')
            else:
                was_prev_present = False

        #Make sure everything adds up
        self.total_primary_premium = row.total_primary_premium
        safe_wc_premium = 0 if np.isnan(row.wc_premium) else row.wc_premium
        safe_gl_premium = 0 if np.isnan(row.gl_premium) else row.gl_premium
        safe_auto_premium = 0 if np.isnan(row.auto_premium) else row.auto_premium
        assert row.total_primary_premium == safe_auto_premium + safe_gl_premium + safe_wc_premium

        #Verify shape
        assert len(casualty_layers) > 0, f'Index {row.Index+1} is empty'
        assert casualty_layers[0].layer == 0
        cumul_size = 0
        for i in range(0, len(casualty_layers)):
            curr = casualty_layers[i]
            if i < len(casualty_layers) - 1:
                next = casualty_layers[i+1]
                assert next.layer == curr.layer + 1, f'Index {row.Index+1} next {next.layer} curr {curr.layer}'
                rate = next.per_mil_prem() / curr.per_mil_prem()
                if (row.Index + 1) not in APPROVED_RATE_INVERSIONS:
                    assert rate <= 1, f'rate inversion for {row.Index+1} btwn {i}&{i+1}'
                assert rate <= 1.25, f'Too extreme of rate inversion with relativity {rate} at id {row.Index+1} btwn {i}&{i+1}'
            assert curr.attachment == cumul_size, f"Index {row.Index+1} layer {i} attach {curr.attachment} cumul {cumul_size} layers {[str(l) for l in casualty_layers]}"
            cumul_size += curr.limit

        layer_sum = sum([l.limit for l in casualty_layers])
        if not np.isnan(row.total_limit):
            assert layer_sum == row.total_limit, f'{layer_sum} vs {row.total_limit} at id {row.Index+1} length {len(casualty_layers)} layers {[str(l) for l in casualty_layers]}'
        self.__total_limit = int(layer_sum)
        self.__casualty_layers = tuple(casualty_layers)
        self.__instantaneous_rate_rel = None
        self.__instantaneous_rate_rel2 = None

    def get_layers(self) -> tuple[LayerPolicy | QuotaShare]:
        return self.__casualty_layers

    def get_layer_count(self) -> int:
        return len(self.__casualty_layers)
    def get_total_limit(self, layer_count: int = None) -> int:
        return self.__total_limit if layer_count is None else sum(map(lambda x: x.limit, self.__casualty_layers[0:layer_count]))
    def get_total_premium(self, layer_count: int = None) -> int:
        layers = self.__casualty_layers if layer_count is None else self.__casualty_layers[0:layer_count]
        return sum(map(lambda x: x.premium, layers))
    def get_carrier_count_at_layer(self, layer: int) -> int:
        return self.__casualty_layers[layer].get_carrier_count()
    def get_total_carrier_count(self) -> int:
        return sum(map(lambda x: x.get_carrier_count(), self.__casualty_layers))
    def get_max_carrier_count(self) -> int:
        return max(map(lambda x: x.get_carrier_count(), self.__casualty_layers))

    def get_lead_per_mil_prem(self) -> int:
        return self.__casualty_layers[0].per_mil_prem()
    def get_per_mil_prem_at_layer(self, layer: int) -> int:
        return self.__casualty_layers[layer].per_mil_prem()
    def get_lead_premium(self) -> int:
        return self.__casualty_layers[0].premium
    
    def get_nth_rate_rel(self, n: int) -> float:
        return self.__casualty_layers[n+1].per_mil_prem() / self.__casualty_layers[n].per_mil_prem()
    
    class InstantaneousModel(NamedTuple):
        a: np.float64
        b: np.float64
        theta: np.float64
        area_perc_error: np.float64
        total_layer_error: np.float64
        p1: float
        p2: float
        layer_count: int

        def __str__(self):
            sign = '+' if self.b > 0 else ''
            return f'{self.a:.3f}*{self.theta:.6f}^x{sign}{self.b:.3f}'
    def __package_instantaneous_rate_rel(
        self,
        total_prem_perc: float,
        p1: float,
        p2: float,
        cand1: tuple[np.float64, np.float64, np.float64],
        cand2: tuple[np.float64, np.float64, np.float64]
    ) -> InstantaneousModel:
        a1, b1, theta1 = cand1
        a2, b2, theta2 = cand2

        theta1_area = ExcessPolicy.AREA_BETWEEN(a1, b1, theta1, 0, 1)
        theta2_area = ExcessPolicy.AREA_BETWEEN(a2, b2, theta2, 0, 1)
        theta1_err = abs(theta1_area - total_prem_perc)
        theta2_err = abs(theta2_area - total_prem_perc)
        
        a, b, theta = cand1 if theta1_err < theta2_err else cand2
        better_area = theta1_area if theta1_err < theta2_err else theta2_area
        perc_err = (better_area - total_prem_perc) / total_prem_perc
        layer_error = 0
        total_limit = self.get_total_limit()
        lead = self.__casualty_layers[0].per_mil_prem()
        for layer in self.__casualty_layers:
            lower_bound = layer.attachment / total_limit
            upper_bound = (layer.attachment + layer.limit) / total_limit
            modeled_area = ExcessPolicy.AREA_BETWEEN(a, b, theta, lower_bound, upper_bound)
            expected_area = (layer.limit / total_limit) * (layer.per_mil_prem() / lead)
            layer_error += abs(modeled_area - expected_area)
        return ExcessPolicy.InstantaneousModel(a, b, theta, perc_err, layer_error, p1, p2, self.get_layer_count())

    def get_instantaneous_rate_rel(self) -> InstantaneousModel:
        if self.__instantaneous_rate_rel is None:
            lead = self.__casualty_layers[0].per_mil_prem()
            phi = self.__casualty_layers[-1].per_mil_prem() / lead
            total_prem_perc = self.get_total_premium() / (self.__total_limit / 1_000_000 * lead)
            f = lambda theta: (phi - theta) / (1 - theta) - (1 - phi) / math.log(theta) - total_prem_perc
            theta1 = least_squares(f, x0=0.03, bounds=(ExcessPolicy.ALMOST_ZERO, ExcessPolicy.ALMOST_ONE)).x[0]
            b1 = (phi - theta1) / (1 - theta1)
            a1 = 1 - b1
            theta2 = least_squares(f, x0=1.03, bounds=(1.0000001, 2.99999999)).x[0]
            b2 = (phi - theta2) / (1 - theta2)
            a2 = 1 - b2
            self.__instantaneous_rate_rel = self.__package_instantaneous_rate_rel(
                total_prem_perc,
                0,
                1,
                (a1, b1, theta1),
                (a2, b2, theta2)
            )
        return self.__instantaneous_rate_rel
    def get_instantaneous_rate_rel2(self, layer_count: int = None, force_theta: float = None) -> InstantaneousModel:
        if self.get_layer_count() == 1: #prev model works with only one layer
            return self.get_instantaneous_rate_rel()
        # if self.__instantaneous_rate_rel2 is not None and self.__instantaneous_rate_rel2.layer_count == layer_count:
        #     return self.__instantaneous_rate_rel2
        
        lead = self.__casualty_layers[0].per_mil_prem()
        total_limit = self.get_total_limit(layer_count=layer_count)
        total_prem_perc = self.get_total_premium(layer_count=layer_count) / (total_limit / 1_000_000 * lead)
        layers = self.__casualty_layers if layer_count is None else self.__casualty_layers[0:layer_count]

        a = None
        b = None
        theta = None
        best_fit = float('inf')
        p1 = None
        p2 = None

        calc_a = lambda theta, y1, y2: (y1 - y2) / (theta ** x1 - theta ** x2)
        calc_b = lambda theta, y1, a: y1 - a * theta ** x1
        def f(theta, y1, y2):
            nonlocal total_prem_perc, lead
            a = calc_a(theta, y1, y2)
            b = calc_b(theta, y1, a)
            return ExcessPolicy.AREA_BETWEEN(a, b, theta, 0, 1) - total_prem_perc

        STEP_SIZE = 20
        for a1 in range(STEP_SIZE+1):
            x1 = a1 / STEP_SIZE
            for a2 in range(a1+1, STEP_SIZE+1):
                x2 = a2 / STEP_SIZE

                layer1 = self.get_layer_index_at_percentage(x1, layer_count=layer_count)
                layer2 = self.get_layer_index_at_percentage(x2, layer_count=layer_count)
                y1 = self.__casualty_layers[layer1].per_mil_prem() / lead
                y2 = self.__casualty_layers[layer2].per_mil_prem() / lead
                f1 = lambda theta: f(theta, y1, y2)
                theta1 = force_theta if force_theta is not None else least_squares(f1, x0=0.03, bounds=(ExcessPolicy.ALMOST_ZERO, ExcessPolicy.ALMOST_ONE)).x[0]
                a1 = calc_a(theta1, y1, y2)
                b1 = calc_b(theta1, y1, a1)

                calc_area_perc = ExcessPolicy.AREA_BETWEEN(a1, b1, theta1, 0, 1)
                if abs(calc_area_perc - total_prem_perc) < 1e-6: #fit correctly
                    layer_error = 0
                    for layer in layers:
                        lower_bound = layer.attachment / total_limit
                        upper_bound = (layer.attachment + layer.limit) / total_limit
                        modeled_area = ExcessPolicy.AREA_BETWEEN(a1, b1, theta1, lower_bound, upper_bound)
                        expected_area = (layer.limit / total_limit) * (layer.per_mil_prem() / lead)
                        layer_error += abs(modeled_area - expected_area)
                    if layer_error < best_fit:
                        best_fit = layer_error
                        a = a1
                        b = b1
                        theta = theta1
                        p1 = x1
                        p2 = x2

        self.__instantaneous_rate_rel2 = ExcessPolicy.InstantaneousModel(
            a,
            b,
            theta,
            area_perc_error=0,
            total_layer_error=best_fit,
            p1=p1,
            p2=p2,
            layer_count=layer_count
        )
        return self.__instantaneous_rate_rel2

    def get_layer_index_at_percentage(self, p: float, layer_count: int = None) -> int:
        assert p >= 0 and p <= 1, f'{p} must be between 0 and 1'
        total_limit = self.get_total_limit(layer_count=layer_count)
        target = int(round(total_limit * p)) #convert to raw dollars
        return self.get_layer_index_at_raw_amount(target)
    def get_layer_index_at_raw_amount(self, a: int) -> int:
        assert a >= 0 and a <= self.__total_limit, f'{a} must be between 0 and total_limit={self.__total_limit}'
        cumul = 0
        for index, layer in enumerate(self.__casualty_layers):
            cumul += layer.limit
            if a <= cumul:
                return index
        raise Exception(f'Could not find premium at amount {a}')



class ClientProgram():
    __tower_cache: list['ClientProgram'] = None

    __MAROON = "#800000"
    __RED = "#e6194b"
    __PINK = "#fabed4"
    __BROWN = "#9a6324"
    __ORANGE = "#f58231"
    __APRICOT = "#ffd8b1"
    __OLIVE = "#808000"
    __YELLOW = "#ffe119"
    __BEIGE = "#fffac8"
    __LIME = "#bfef45"
    __GREEN = "#3cb44b"
    __MINT = "#aaffc3"
    __TEAL = "#469990"
    __CYAN = "#42d4f4"
    __NAVY = "#000075"
    __BLUE = "#4363d8"
    __PURPLE = "#911eb4"
    __LAVENDER = "#dcbeff"
    __MAGENTA = "#f032e6"
    COLORS = (
        __BLUE, __GREEN, __RED,
        __CYAN, __MAGENTA, __ORANGE,
        __YELLOW, __PURPLE, __PINK,
        __BROWN, __NAVY, __TEAL,
        __APRICOT, __MAROON, __LIME,
        __LAVENDER, __MINT, __BEIGE,
        __OLIVE
    )

    TARGET_DOT_COUNT = 75
    ROUND_DOT_COUNT = 1_000_000

    def __init__(self, row: ClientRow):
        self.id = row.Index + 1
        self.sic_code = DEFAULT_SIC_CODE if np.isnan(float(row.sic_code)) else SicCode(float(row.sic_code))
        self.region = row.region
        self.pseudonym = f'Company {self.id} ({self.region})'

        self.auto_policy = None
        if all_are_some([row.auto_carrier, row.auto_deductible, row.auto_premium, row.auto_loss_pick_at_retention, row.fleet_count]):
            self.auto_policy = AutoPolicy(row.auto_carrier, row.auto_premium, row.auto_deductible, row.auto_loss_pick_at_retention, row.fleet_count)
        self.wc_policy = None
        if all_are_some([row.wc_carrier, row.wc_deductible, row.wc_premium, row.wc_loss_pick_at_retention]):
            self.wc_policy = DeductiblePolicy(row.wc_carrier, row.wc_premium, row.wc_deductible, row.wc_loss_pick_at_retention)
        self.gl_policy = None
        if all_are_some([row.gl_carrier, row.gl_deductible, row.gl_premium, row.gl_loss_pick_at_retention]):
            self.gl_policy = DeductiblePolicy(row.gl_carrier, row.gl_premium, row.gl_deductible, row.gl_loss_pick_at_retention)

        self.buffer_policy = None
        if all_are_some([row.buffer_carrier, row.buffer_limit, row.buffer_premium]):
            self.buffer_policy = LayerPolicy(-1, row.buffer_carrier, row.buffer_premium, row.buffer_limit, 0)

        try:
            self.excess_policy = ExcessPolicy(row)
            assert self.sic_code.get_major_industry_name() != SicCode.NON_CLASSIFIABLE, f'Client {self.id} needs SIC code'
        except:
            self.excess_policy = None
    def __str__(self):
        if self.excess_policy is not None:
            return f'Company {self.id}: limit ${self.excess_policy.get_total_limit()/1_000_000}M'
        else:
            return f'Company {self.id}: no valid excess'

    @staticmethod
    def load_all() -> list['ClientProgram']:
        for x in APPROVED_RATE_INVERSIONS:
            assert x not in MALFORMED_ROWS, f'Approved row inversion {x} in MALFORMED_ROWS'
        if ClientProgram.__tower_cache is None:
            import pandas as pd
            tower_data = pd.read_excel('4 1 25 MS Lists run.xlsx')
            ClientProgram.__tower_cache = []
            for row in tower_data.itertuples():
                row: ClientRow = row
                tower = ClientProgram(row)
                if row.Index + 1 in MALFORMED_ROWS or row.Index + 1 in EMPTY_ROWS:
                    assert tower.excess_policy is None, f'Tower at id {row.Index+1} had valid excess policy'
                else:
                    assert tower.excess_policy is not None, f'Tower at id {row.Index+1} missing valid excess policy'
                ClientProgram.__tower_cache.append(tower)
        return ClientProgram.__tower_cache
    @staticmethod
    def query(
        has_tower: bool = None,
        has_useful_tower: bool = None,
        has_tower_of_size: int = None,
        has_auto: bool = None,
        has_wc: bool = None,
        has_gl: bool = None,
        has_quota_share_of_length: int = None,
        industry: str = None
    ) -> list['ClientProgram']:
        matching_towers = ClientProgram.load_all()

        if has_tower:
            matching_towers = filter(lambda x: x.excess_policy is not None, matching_towers)
        elif has_tower == False:
            matching_towers = filter(lambda x: x.excess_policy is None, matching_towers)
        
        if has_useful_tower:
            matching_towers = filter(lambda x: x.excess_policy is not None and x.excess_policy.get_layer_count() > 1, matching_towers)
        elif has_useful_tower == False:
            matching_towers = filter(lambda x: x.excess_policy is None or x.excess_policy.get_layer_count() == 1, matching_towers)
       
        if has_tower_of_size:
            matching_towers = filter(lambda x: x.excess_policy is not None and x.excess_policy.get_layer_count() >= has_tower_of_size, matching_towers)

        if has_auto:
            matching_towers = filter(lambda x: x.auto_policy is not None, matching_towers)
        elif has_auto == False:
            matching_towers = filter(lambda x: x.auto_policy is None, matching_towers)

        if has_gl:
            matching_towers = filter(lambda x: x.gl_policy is not None, matching_towers)
        elif has_gl == False:
            matching_towers = filter(lambda x: x.gl_policy is None, matching_towers)
        
        if has_wc:
            matching_towers = filter(lambda x: x.wc_policy is not None, matching_towers)
        elif has_wc == False:
            matching_towers = filter(lambda x: x.wc_policy is None, matching_towers)

        if has_quota_share_of_length == 1:
            matching_towers = filter(lambda x: x.excess_policy is not None and x.excess_policy.get_max_carrier_count() == 1, matching_towers)
        elif has_quota_share_of_length is not None and has_quota_share_of_length > 1:
            matching_towers = filter(lambda x: x.excess_policy is not None and x.excess_policy.get_max_carrier_count() >= has_quota_share_of_length, matching_towers)

        if industry is not None:
            matching_towers = filter(lambda x: x.sic_code.get_major_industry_name() == industry, matching_towers)

        return list(matching_towers)
    @staticmethod
    def query_test_split(
        has_tower: bool = None,
        has_useful_tower: bool = None,
        has_tower_of_size: int = None,
        has_auto: bool = None,
        has_wc: bool = None,
        has_gl: bool = None,
        has_quota_share_of_length: int = None,
        industry: str = None,
        random_state = 42,
        test_size: float = 0.2
    ) -> tuple[tuple['ClientProgram'], tuple['ClientProgram']]:
        filter_kwargs = {
            k: v for k, v in locals().items()
            if k not in ('random_state', 'test_size')
        }
        matching_towers = ClientProgram.query(**filter_kwargs)
        try:
            X_train, X_test, _, _ = train_test_split(matching_towers, [0] * len(matching_towers), test_size=test_size, random_state=random_state)
            return (tuple(X_train), tuple(X_test))
        except ValueError as _:
            return (tuple(matching_towers), tuple())

    @staticmethod
    def all_to_csv(file_name: str, collapse_quota_shares: bool = True):
        open(file_name, 'w').write('')
        output = open(file_name, 'a')
        output.write('client_id,carrier_number,layer_number,limit,attachment,share,rate_rel,lead_ratio,per_mil,premium,layers_in_tower,entries_in_tower,sic_industry,carrier_name,is_train1,is_train2,is_train3\n')
        train1, test1 = ClientProgram.query_test_split(has_tower=True)
        train2, test2 = ClientProgram.query_test_split(has_tower=True, random_state=6)
        train3, test3 = ClientProgram.query_test_split(has_tower=True, random_state=312)
        all_towers = sorted([*train1, *test1], key=lambda x: x.id)
        training_ids1 = list(map(lambda x: x.id, train1))
        training_ids2 = list(map(lambda x: x.id, train2))
        training_ids3 = list(map(lambda x: x.id, train3))
        for prog in all_towers:
            is_train1 = 1 if (prog.id in training_ids1) else 0
            is_train2 = 1 if (prog.id in training_ids2) else 0
            is_train3 = 1 if (prog.id in training_ids3) else 0
            entry_count = 0
            layers = prog.excess_policy.get_layers()
            sic_sector = prog.sic_code.get_major_industry_name()
            if ',' in sic_sector:
                sic_sector = f'"{sic_sector}"'
            lead_ppm = prog.excess_policy.get_lead_per_mil_prem()

            for layer_index, layer in enumerate(layers): #count total row entries
                if isinstance(layer, LayerPolicy) or collapse_quota_shares:
                    entry_count += 1
                else:
                    entry_count += len(layer.get_shares())
                        
            for layer_index, layer in enumerate(layers):
                rate_rel = '' if layer_index == 0 else round(prog.excess_policy.get_nth_rate_rel(layer_index-1), 3)
                lead_ratio = layer.per_mil_prem() / lead_ppm
                if isinstance(layer, LayerPolicy) or collapse_quota_shares:
                    output.write(f'{prog.id},1,{layer_index},{layer.limit},{layer.attachment},1,{rate_rel},{lead_ratio},{round(layer.per_mil_prem(), 3)},{layer.premium},{len(layers)},{entry_count},{sic_sector},"{layer.get_carrier_str()}",{is_train1},{is_train2},{is_train3}\n')
                else:
                    for share_index, share in enumerate(layer.get_shares()):
                        output.write(f'{prog.id},{share_index+1},{layer_index},{share.limit},{share.attachment},{round(share.limit/layer.limit, 3)},{rate_rel},{lead_ratio},{round(share.per_mil_prem(), 3)},{share.premium},{len(layers)},{entry_count},{sic_sector},"{share.get_carrier_str()}",{is_train1},{is_train2},{is_train3}\n')
    @staticmethod
    def all_to_std_csv(file_name: str, collapse_quota_shares: bool = True):
        open(file_name, 'w').write('')
        output = open(file_name, 'a')
        #ClientID,Line,LayerNumber,CarrierNumber,Carrier,Attachment,Limit,Capacity,Premium,PPM
        output.write('ClientID,Line,LayerNumber,CarrierNumber,Carrier,Attachment,Limit,Capacity,Premium,PPM\n')
        clean_carrier = lambda s: 'Placeholder' if s is None else (f'"{s}"' if ',' in s else s)
        def clean_number(n):
            s = f'{int(n):,}'
            return f'"{s}"' if ',' in s else s
        for prog in ClientProgram.query(has_tower=True):
            for layer_index, layer in enumerate(prog.excess_policy.get_layers()):
                rate_rel = '' if layer_index == 0 else round(prog.excess_policy.get_nth_rate_rel(layer_index-1), 3)
                line = 'UM' if layer_index == 0 else 'XS'
                if isinstance(layer, LayerPolicy) or collapse_quota_shares:
                    output.write(f'{prog.id},{line},{layer_index},,{clean_carrier(layer.get_carrier_str())},{clean_number(layer.attachment)},{clean_number(layer.limit)},{clean_number(layer.limit+layer.attachment)},{clean_number(layer.premium)},{clean_number(layer.per_mil_prem())}\n')
                else:
                    cumul = 0
                    for share_index, share in enumerate(layer.get_shares()):
                        cumul += share.limit
                        output.write(f'{prog.id},{line},{layer_index},,{clean_carrier(layer.get_carrier_str())},{clean_number(share.attachment)},{clean_number(share.limit)},{clean_number(cumul+share.attachment)},{clean_number(share.premium)},{clean_number(share.per_mil_prem())}\n')

    @staticmethod
    def from_id(id: int) -> Optional['ClientProgram']:
        try:
            index = list(map(lambda x: x.id, ClientProgram.load_all())).index(id)
            return ClientProgram.__tower_cache[index]
        except:
            return None

    @staticmethod
    def visualize_industry_rate_rel(industry: str):
        relevant_towers = ClientProgram.query(industry=industry, has_useful_tower=True)
        fig, ax = plt.subplots()
        points = [[0, 0] for _ in range(21)]
        ax.plot(0, 0, color='white')
        tower_count_max = 0
        for index, tower in enumerate(relevant_towers):
            layer_count = tower.excess_policy.get_layer_count()
            if layer_count < 2:
                continue
            tower_count_max = max(tower_count_max, layer_count)
            for l in range(0, layer_count - 1):
                points[l][0] += tower.excess_policy.get_nth_rate_rel(l)
                points[l][1] += 1
            tower.visualize_rate_rel(ClientProgram.COLORS[index % len(ClientProgram.COLORS)], ax)

        for index, s in enumerate(points):
            total, count = s
            if count > 0:
                ax.plot(index, total/count, marker='x', color='black', ms=10)

        ax.plot(tower_count_max+3, 0, color='white')
        plt.title(industry)
        plt.legend()
        plt.show()
    
    def get_wtw_industry_index(self) -> int:
        return ClientProgram.ALL_INDUSTRIES.index(self.industry)
    
    def visualize_rate_rel(self, color: str = 'black', ax: axes.Axes = None):
        had_axes = ax is not None
        if ax is None:
            fig, ax2 = plt.subplots()
            ax = ax2
            ax.plot(0, 0, color='white')
        if not had_axes and self.excess_policy.get_layer_count() == 0:
            ax.plot(0, 0, color='red', marker='*')
        for i in range(self.excess_policy.get_layer_count() - 1):
            rate_rel = self.excess_policy.get_nth_rate_rel(i)
            marker = '*' if rate_rel > 1 else 'o'
            if i == 0:
                ax.plot(i, rate_rel, marker=marker, color=color, label=self.pseudonym)
            else:
                ax.plot(i, rate_rel, marker=marker, color=color)
        if not had_axes:
            ax.set_title(f'({self.id}) {self.pseudonym or "Unnamed"}')
            plt.show()
    def visualize(self, color: str = 'black', ax: axes.Axes = None, model_layer_count: int = None):
        had_axes = ax is not None
        if ax is None:
            fig, ax2 = plt.subplots()
            ax = ax2
        
        lead_per_mil = self.excess_policy.get_layers()[0].per_mil_prem()
        ax.plot(lead_per_mil, self.excess_policy.get_total_limit())

        cumul_y = 0
        for layer_index, layer in enumerate(self.excess_policy.get_layers()):
            cumul_x = 0
            base_color = '#000080' if layer_index%2==0 else '#60d060'
            for share_index, share in enumerate(layer.get_shares()):
                facecolor = base_color if share_index%2==0 else 'white'
                edgecolor = base_color if share_index%2==1 else 'none'
                width = (share.limit / layer.limit) * layer.per_mil_prem()
                ax.add_patch(Rectangle((cumul_x, cumul_y), width=width, height=layer.limit, edgecolor=edgecolor, facecolor=facecolor))
                cumul_x += width
            cumul_y += layer.limit
        a, b, theta, _, layer_error, p1, p2, _ = self.excess_policy.get_instantaneous_rate_rel2(layer_count=model_layer_count)
        model_limit = self.excess_policy.get_total_limit(layer_count=model_layer_count)
        total_limit = self.excess_policy.get_total_limit()
        for i in range(61):
            p = i / 50 * total_limit / model_limit
            pred = a * theta ** p + b
            x = pred * lead_per_mil
            y = p * model_limit
            marker = 'o' if y <= model_limit else 'x'
            ax.plot(x, y, color='lightgrey', marker=marker)


        if not had_axes:
            # plt.hlines(
            #     [p1 * model_limit, p2 * model_limit],
            #     [0, 0],
            #     [lead_per_mil, lead_per_mil]
            # )
            if model_layer_count is not None:
                plt.hlines(
                    [model_limit],
                    [0],
                    [lead_per_mil],
                    linestyle=':',
                    color='black'
                )
            plt.xlabel("Per Million Pricing ($)")
            plt.ylabel("Total Program Limit ($)")
            dollar_error = int(layer_error * lead_per_mil * self.excess_policy.get_total_limit() / 1_000_000)
            total_premium = int(self.excess_policy.get_total_premium())
            dollar_percent = dollar_error / total_premium * 100
            sb = f'{b:.3f}' if b < 0 else f'+{b:.3f}'
            plt.title(rf'{self} | {a:.3f}*{theta:.6f}^x{sb} (\${dollar_error:,}/\${total_premium:,}; {dollar_percent:.1f}%) [err: {layer_error:.4f}] using {"all" if model_layer_count is None else model_layer_count} layers')
            plt.show()



if __name__ == "__main__":
    ClientProgram.load_all()
